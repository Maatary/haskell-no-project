# Overview

This project is part of an introductory work on how to compile and use dependencies in haskell project without resorting to packaging, which falls into [my overall haskell learning journey project](https://github.com/Maatary/haskell-from-first-principles). 

# Appraoch

## Compiling Haskell Sources With No External Dependencies to Produce an Executable Without Resorting to Packaging

Compiling the haskell sources of a project, without resorting to packaging is done as follows:

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
     $ ghc Test.hs -main-is Test
    ```

 - Few things are happening here: 
  
   - With `ghc Test.hs` we tell GHC what source file to compile. With `-main-is Test` we tell GHC what is the Module containing the main function. 


   - GHC ccompile the code into an inteface file Test.hi and library object file Test.o and then link Library Object file into an executable file Test. We can run the executable as follows:
        ```shell
        $ ./Test
        Hello, Maat and welcome to Haskell!
        ```
  
   - Typically the convention is to call the module containing the **main function** **Main**, and the containing file as per haskell module rules **Main.hs**. If done according to the convention we could have simply type
        ```shell
        $ ghc Main.hs 
        ``` 

    - However for the **Main module** **the rule for "module" / "source file" strict name correspondance** are actually relaxed. So we could have had the file called **Test.hs** and the module inside it **Main** and then type
        ```shell
        $ ghc Test.hs 
        ```

    - Moreover we could have simply ommit the **Main** module inside the **Test.hs** and type the same as above. When that is the case, GHC consider that there is an implicit Main module.
        ```haskell
        import Greetings.SayHello

        main :: IO ()
        main = sayHello "Maat"
        ```


    - It is important to note if none the rules on specifying the main function explicitly or implicitly as explained  above, and we simply type `ghc Test.hs` where in the **Test.hs** we have the **Test Module define** as in our current code base, then GHC would not generate an executable but simply compile the sources into the interface files and library object files.
  
    - Finally when the program contains multiple modules, such as in our codebase, then we only need to tell GHC the name of the source file containing the Main module, and GHC will examine the import declarations to find the other modules that make up the program and find their source files. This means that, with the exception of the Main module, every source file should be named after the module name that it contains (with dots replaced by directory separators). That's why Greetings.SayHello, which as per haskell namespace rules is in Greeting/SayHello can be found by GHC during compilation. For more on module hiearchy and namespace please read the main learning project which this project complement https://github.com/Maatary/haskell-from-first-principles#module-hierarchy-namespaces-and-directory-structure


    - References
      - https://downloads.haskell.org/ghc/9.0.2/docs/html/users_guide/using.html#using-ghc
      - https://downloads.haskell.org/ghc/9.0.2/docs/html/users_guide/using.html#modes-of-operation
      - https://downloads.haskell.org/ghc/9.0.2/docs/html/users_guide/using.html#make-mode
      - https://downloads.haskell.org/ghc/9.0.2/docs/html/users_guide/phases.html?highlight=main#ghc-flag--main-is%20%E2%9F%A8thing%E2%9F%A9
      - https://stackoverflow.com/questions/46895199/no-output-will-be-generated-because-there-is-no-main-module
      - https://stackoverflow.com/questions/11112371/to-write-or-not-to-write-module-main-where-in-haskell
      - https://www.haskell.org/onlinereport/haskell2010/haskellch5.html (Abbreviated form for the module Main)

## Compiling Haskell Sources With External Dependencies to Produce an Executable Without Resorting to Packaging

Compiling the haskell sources of a project with external dependencies, without resorting to packaging, is done as follows:

- First, from within the current project (haskell-no-project), download the depenendency manually and unpack it.

  - For this demo we will be using the **TimeIt** library which can be found on [hackage](https://hackage.haskell.org/package/timeit) or better [stackage](https://www.stackage.org/lts-19.24/package/timeit-2.0). It provides for the functionality to time functions (monadic functions).
  
  - **It is important to note at this point that, in haskell, code which represent library, are made available online as package. In other words, code intended to be used by other as distributed as package. A Package among other, play the role of a unit of distribution in Haskell.**
  
  - **Going into the detail of how package and packaging works is beyond the scope of this project, as the goal is to somewhat explain compilation without them.** However because we need to work with a dependency, such as to illustrate how to work with an external code, we had to download one, because in haskell anything that is available online is packaged. Nonetheless, as we will be illustrating further below, we will not use the haskell packaging appraoch. We will unpack it, and **"work as if"** it was just an external code that was not packaged originally. We say "work as if" because to speed up the all process **we will make a  minimal use of the packaging facility**, althought that is low level enough to keep being able to illustrate our point. Without it, the real process would be somewhat long and painfull. We will however provide references pointing to how to do it fully bare along the way.

  - If you would like to open the timeIT project independenly of this project, using the LHS extension for VsCode to benefit from IDE assistance while browsing the code, you have the option to use my [fork of the TimeIt project](https://github.com/Maatary/timeit) which has been adapted to work smoothly with LHS extension for VSCode. The ReadMe of the project explain the adaptation.

      ```shell
      $ curl http://hackage.haskell.org/package/timeit-2.0/timeit-2.0.tar.gz --output timeit-2.0.tar.gz                                              
      $ tar -xf timeit-2.0.tar.gz                                                
      $ cd timeit-2.0
      ```

- The project tree should now look like so

   ![project tree](full-project-tree.png)