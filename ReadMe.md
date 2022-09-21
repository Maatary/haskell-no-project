# Overview

This project is part of an introductory work on how to use dependencies in haskell project without resorting to packaging, which falls into [my overall haskell learning journey project](https://github.com/Maatary/haskell-from-first-principles). 

# Appraoch

Use dependencies in haskell project without resorting to packaging is applied as follows:

 - First we create non-package managed project a.k.a. a non-source package project. This is essentially a folder where we have our haskell source file.

    ![project tree](initial-project-tree.png)

 - Note that, as can be seen in the picture above we have a namespaced module Greetings.SayHello which follow the haskell convention expected by GHC, requireing an exact match between namespace and folder. Hence the Greetings folder and SayHello Module in it. We do this with the intent to also demonstrate/explain how during compilation ghc finds sources _(see below)_. 


 - We also intentionally name our main module Test and locate it in the file Test.hs, again with the intent to explain some implicit but overlooked rule of compilation with ghc.

 - The code is quite simple at this point

    ```hakell
     module Test where
  
        import Greetings.SayHello

        main :: IO ()
        main = sayHello "Maat"
    ```

    ```hakell
     module Greetings.SayHello where

        sayHello :: String -> IO ()
        sayHello x =
        putStrLn ("Hello, " ++ x ++ " and welcome to Haskell!")
    ```

 - Now we show how to compile the project without dependencies in order to produce an executable

    ```bash
     ghc Test.hs -main-is Test
    ```

 - Few things are happening here: 
   - GHC ccompile the code into an inteface file Test.hi and library object file Test.o and then link Library Object file into an executable file Test.
  
   - Typically the convention is to call the module containing the main function Main, and the containing file as per haskell module rules Main.hs. If done according to the convention we could have simply type
      ```bash
      ghc Main.hs 
      ``` 

    - However for the **Main module** **the rule for "module" / "source file" strict name correspondance** are actually relaxed. So we could have had the file called **Test.hs** and the module inside it **Main** and then type
      ```bash
      ghc Test.hs 
      ```

    - Futhermore could have simply ommit the **Main** module inside the **Test.hs** and type the same as above. When that is the case, GHC consider that there is an implicit Main module.


    - Finally it is important to note if none the rules on specifying the main function explicitly or implicitly as explained  above, and we simply type `ghc Test.hs` where in the **Test.hs** we have the **Test Module define** as in our current code base, then GHC would not generate an executable but simply compile the sources into the interface files and library object files.

 - Download a depenendency manually
   - In the demo we use my fork of the timeit project located on github. 
   - Typically however, we would download its tarball from [hackage](https://hackage.haskell.org/package/timeit) or better [stackage]([https://](https://www.stackage.org/lts-19.24/package/timeit-2.0)) and unpack it, in its folder within out current haskell source folder.
