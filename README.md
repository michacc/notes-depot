## NotesDepot

A simple website for storing notes based on Yesod framework. All text on the
website is in Polish.

### Installation of required packages

1. Download and install the Haskell Platform from http://haskell.org. If you
   use Linux, there probably is a package that contains the platform and all
   you have to do is installing that package. For example, on Ubuntu, those
   commands will install the Haskell Platform:

   ```
   $ sudo apt-get update
   $ sudo apt-get install haskell-platform
   ```

2. Install the newest version of the cabal utility:

   ```
   $ cabal update
   $ cabal install cabal-install
   ```

3. Add the directory with your local binaries of packages installed by cabal
   to PATH environment variable. Locations of the directory:
      * Linux: `~/.cabal/bin`
      * OS X: `~/Library/Haskell/bin`
      * Windows: `%APPDATA%\cabal\bin`

4. Execute the following commands:

   ```
   $ cabal install alex happy
   $ cabal install yesod-platform yesod-bin --reorder-goals --max-backjumps=-1
   ```

   If the installation fails because of some package dependencies, you may try
   installing these packages in the sandbox created for the cloned repository
   in the next section. However, then, during execution of `yesod devel`, the
   directory with sandbox binaries should be in PATH.


### Running the website

1. Clone the repository, create a new package environment and run yesod with
   devel command:

   ```
   $ git clone https://github.com/michacc/notes-depot.git
   $ cd notes-depot
   $ cabal sandbox init
   $ cabal install --only-dependencies --reorder-goals --force-reinstalls
   $ yesod devel
   ```

   The last but one command may take same time to complete.

2. When you see in the terminal line:

   ```
   Devel application launched: http://localhost:3000
   ```

   after executing `yesod devel`, open in a browser the page
   http://localhost:3000. Now, you can browse the website.
