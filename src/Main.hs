{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Graphics.UI.UrzaStarter as Urza
import           Control.Concurrent.MVar
import           Graphics.Rendering.OpenGL hiding (DisplayList, Matrix, renderer, get)
import           Graphics.Rendering.Draw
import           Graphics.Rendering.Draw.Math
import           Graphics.Rendering.Draw.Utils
import           Graphics.Rendering.Draw.Shader.Text as T
import           Graphics.Rendering.Draw.Shader.Shape as S
import           Graphics.Rendering.Draw.Text.Renderer
import           Graphics.Rendering.Draw.Text.Types
import           Graphics.Rendering.Draw.LoadTexture
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           System.Exit
import           System.Directory
import           Data.Maybe
import           Data.Monoid
import qualified Data.List as L
import qualified Data.Map as M
import Debug.Trace


data NodeRenderer = NodeRenderer { _rendererWindowSize :: Size
                                 , _rendererModelview  :: Matrix GLfloat
                                 , _rendererShapeShader:: ShapeShaderProgram
                                 , _rendererTextShader :: TextShaderProgram
                                 , _rendererTextAtlas  :: Atlas
                                 }
makeLenses ''NodeRenderer


type Draw = NodeRenderer -> IO ()


data Scale = Scale GLfloat GLfloat


data Rotation = Rotation GLfloat


data DisplayNode = DisplayNode { _nodeName     :: String
                               , _nodeTexture  :: Maybe TextureObject
                               , _nodePosition :: Position
                               , _nodeSize     :: Size
                               , _nodeScale    :: Scale
                               , _nodeRotation :: Rotation
                               , _nodeDraws    :: [Draw]
                               , _nodeChildren :: Int
                               }
makeLenses ''DisplayNode


emptyNode :: DisplayNode
emptyNode = DisplayNode { _nodeName = ""
                        , _nodeTexture = Nothing
                        , _nodePosition = Position 0 0
                        , _nodeSize = Size 0 0
                        , _nodeScale = Scale 1 1
                        , _nodeRotation = Rotation 0
                        , _nodeDraws = []
                        , _nodeChildren = 0
                        }


nodeTransform :: DisplayNode -> Matrix GLfloat
nodeTransform n =
    let Position tx ty = n^.nodePosition
        Rotation rz    = n^.nodeRotation
        Scale sx sy    = n^.nodeScale
        t = translationMatrix3d (fromIntegral tx) (fromIntegral ty) 0.0
        r = rotationMatrix3d 0 0 rz
        s = scaleMatrix3d sx sy 1
    in foldl multiply (identityN 4) [t,r,s]


type DisplayList = [DisplayNode]


node :: State DisplayNode DisplayList -> DisplayList
node s = let (list,n) = runState s emptyNode
             n' = n{_nodeChildren = length list}
         in n':list


drawShapes :: IO () -> State DisplayNode ()
drawShapes f = nodeDraws %= (++ [renderShapes f])


renderShapes :: IO () -> Draw
renderShapes f r = do
    let Size w h = r^.rendererWindowSize
        pj  = orthoMatrix 0 (fromIntegral w) 0 (fromIntegral h) 0 1
        mv  = r^.rendererModelview
        s   = r^.rendererShapeShader
    currentProgram $= Just (s^.S.program)
    s^.setIsTextured $ False
    s^.S.setProjection $ concat pj
    s^.S.setModelview $ concat mv
    f


renderList :: NodeRenderer -> DisplayList -> IO DisplayList
renderList _ [] = return []
renderList r (n:list) = do
    let nodes = take (n^.nodeChildren) list
        rest  = drop (n^.nodeChildren) list
        r'    = r & rendererModelview %~ (`multiply` nodeTransform n)
    -- Draw this node
    forM_ (n^.nodeDraws) $ \f -> f r'
    -- Draw subnodes with this modelview.
    nodes' <- renderList r' nodes
    -- Draw the rest of the nodes without this modelview.
    rest' <- renderList r rest
    return $ [n] ++ nodes' ++ rest'


data App = App { _appCursor :: Position }


newApp :: App
newApp = App (Position 0 0)


gui :: DisplayList
gui = node $ do
    -- Set some initial goodies.
    nodeSize .= Size 150 150
    nodePosition .= Position 50 50
    -- Do some drawing into the node.
    drawShapes $ strokePath $ do
        setColor $ Color4 1 0 0 1
        moveTo 0 0
        lineTo 150 150
        rectangleAt 0 0 150 150
    -- Return the child nodes.
    return $ (node $ do
                 nodePosition .= Position 50 50
                 drawShapes $ fillPath $ do
                     setColor $ Color4 0 1 0 1
                     rectangleAt 0 0 50 50
                 return [])
             ++
             (node $ do
                 nodePosition .= Position 50 100
                 nodeRotation .= (Rotation 1)
                 drawShapes $ fillPath $ do
                     setColor $ Color4 0 0 1 1
                     rectangleAt 0 0 50 50
                 return $ node $ do
                              nodePosition .= Position 50 50
                              drawShapes $ fillPath $ do
                                  setColor $ Color4 1 1 0 1
                                  rectangleAt 0 0 50 50
                              drawShapes $ strokePath $ do
                                  setColor $ Color4 0 1 1 1
                                  rectangleAt 0 0 50 50
                              return [])


main :: IO ()
main = do
    wvar <- initUrza (100,100) (800,600) "Purely Functional Display List"
    font <- fmap (++ "/assets/font/Deutsch.ttf") getCurrentDirectory
    print font

    let load tr = loadCharMap tr $ map toEnum [33..126]
    (TextRenderer tshader atlas) <- makeTextRenderer font 32 >>= load

    sshader <- makeShapeShaderProgram

    guiVar <- newMVar gui

    forever $ do
        pollEvents
        (_, window) <- takeMVar wvar
        (winW, winH) <- fmap (over both fromIntegral) $ getWindowSize window

        makeContextCurrent $ Just window
        viewport $= (Position 0 0, Size winW winH)
        blend $= Enabled
        blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
        clearColor $= Color4 0.13 0.13 0.13 1
        clear [ColorBuffer, DepthBuffer]

        let renderer = NodeRenderer { _rendererWindowSize  = Size winW winH
                                    , _rendererModelview   = identityN 4
                                    , _rendererShapeShader = sshader
                                    , _rendererTextShader  = tshader
                                    , _rendererTextAtlas   = atlas
                                    }

        -- Render the display list.
        modifyMVar_ guiVar $ renderList renderer

        swapBuffers window
        shouldClose <- windowShouldClose window
        putMVar wvar ([],window)
        when shouldClose exitSuccess


