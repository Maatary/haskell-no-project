# Overview

This project is part of an introductory work on how to compile and use dependencies in haskell project without resorting to packaging, which falls into [my overall haskell learning journey project](https://github.com/Maatary/haskell-from-first-principles). 

# Appraoch

## Compiling Haskell Sources With No External Dependencies to Produce an Executable Without Resorting to Packaging

compiling haskell sources project without resorting to packaging is done as follows:

 - First we create non-package managed project a.k.a. a non-source package project. This is essentially a folder where we have our haskell source files.

    ![project tree](initial-project-tree.png)

 - Note that, as can be seen in the picture above we have a namespaced module Greetings.SayHello which follow the haskell convention expected by GHC, requiring an exact match between namespace and folder. Hence the Greetings folder and SayHello Module in it. We do this with the intent to also demonstrate/explain how during compilation ghc finds sources _(see below)_. 


 - We also intentionally name the module that contain the **main function** Test and locate it in the file Test.hs, again with the intent to explain some implicit but overlooked rules of compilation with ghc. Those rules are typically overlooked because when working with haskell packaging systems, convention are automatically applied when those tools scafold projects, hence, we just never pay attention to it. However certain projects may requires some specific setups, at that point, being aware of those rules is paramount.

 - The code is quite simple at this point

    ```haskell
     module Test where
  
        import Greetings.SayHello

        main :: IO ()
        main = sayHello "Maat"
    ```

    ```haskell
     module Greetings.SayHello where
        
        sayHello :: String -> IO ()
        sayHello x =
        putStrLn ("Hello, " ++ x ++ " and welcome to Haskell!")
    ```

 - Now we show how to compile the project without dependencies in order to produce an executable

    ```shell
     ghc Test.hs -main-is Test
    ```

 - Few things are happening here: 
  
   - With `ghc Test.hs` we tell GHC what source file to compile. With `-main-is Test` we tell GHC what is the Module containing the main function. 


   - GHC ccompile the code into an inteface file Test.hi and library object file Test.o and then link Library Object file into an executable file Test. We can run the executable as follows:
        ```shell
        ./Test
        Hello, Maat and welcome to Haskell!
        ```
  
   - Typically the convention is to call the module containing the **main function** **Main**, and the containing file as per haskell module rules **Main.hs**. If done according to the convention we could have simply type
        ```shell
        ghc Main.hs 
        ``` 

    - However for the **Main module** **the rule for "module" / "source file" strict name correspondance** are actually relaxed. So we could have had the file called **Test.hs** and the module inside it **Main** and then type
        ```shell
         ghc Test.hs 
        ```

    - Moreover we could have simply ommit the **Main** module inside the **Test.hs** and type the same as above. When that is the case, GHC consider that there is an implicit Main module.
        ```haskell
        import Greetings.SayHello

        main :: IO ()
        main = sayHello "Maat"
        ```


    - It is important to note if none the rules on specifying the main function explicitly or implicitly as explained  above, and we simply type `ghc Test.hs` where in the **Test.hs** we have the **Test Module define** as in our current code base, then GHC would not generate an executable but simply compile the sources into the interface files and library object files.
  
    - Finally when the program contains multiple modules  such as in our codebase, then we only need to tell GHC the name of the source file containing the Main module, and GHC will examine the import declarations to find the other modules that make up the program and find their source files. This means that, with the exception of the Main module, every source file should be named after the module name that it contains (with dots replaced by directory separators). That why Greetings.SayHello is in Greeting/SayHello and GHC can find it.


    - References
      - https://downloads.haskell.org/ghc/9.0.2/docs/html/users_guide/using.html#using-ghc
      - https://downloads.haskell.org/ghc/9.0.2/docs/html/users_guide/using.html#modes-of-operation
      - https://downloads.haskell.org/ghc/9.0.2/docs/html/users_guide/using.html#make-mode
      - https://downloads.haskell.org/ghc/9.0.2/docs/html/users_guide/phases.html?highlight=main#ghc-flag--main-is%20%E2%9F%A8thing%E2%9F%A9
      - https://www.haskell.org/onlinereport/haskell2010/haskellch5.html (Abbreviated form for the module Main)
      - https://stackoverflow.com/questions/11112371/to-write-or-not-to-write-module-main-where-in-haskell

## Compiling Haskell Sources With External Dependencies to Produce an Executable Without Resorting to Packaging

 - Download a depenendency manually
   - In the demo we use my fork of the timeit project located on github. 
   - Typically however, we would download its tarball from [hackage](https://hackage.haskell.org/package/timeit) or better [stackage]([https://](https://www.stackage.org/lts-19.24/package/timeit-2.0)) and unpack it, in its folder within out current haskell source folder.
