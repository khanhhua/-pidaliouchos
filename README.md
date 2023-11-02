timonieris
====

This is supposed to be a toy project for me to practise Haskell.
The following concepts of Haskell shall be explored:

- Composition
- Extensibility
- Multithreaded environment

So what is this project about? The name __timonieris__ is a Greek word for
"steerman", which means you should be able to orchestrate defined list of tasks. Task file is found at your home folder under **.timonieris**

## Synopsis
```
cabal run timonieris -- <relative path to timo file>
```
For example
```
cabal run timonieris -- examples/singles.timo
```

or prepare a timo file named **.timonieris** in your home directory and simply go `cabal run timonieris`.

## Task definition syntax:

Very simple! Tasks can either be executed in sequence or in parallel groups.

- **In sequence**
```
echo "HELLO WORLD!"
ls /usr/share/
```

- **In parallel**
```
apt-get update -y

port selfupdate

git clone https://github.com/github/docs.git
```

- **In parallel groups**
```
echo "HELLO WORLD!"
ls /usr/share/

apt-get update -y
port selfupdate
git clone https://github.com/github/docs.git
```

Each task group is monitored independently. When a task or step in a group fails (i.e. exit code is none zero), timonieris will retry indefinitely :D until all tasks are finished successfully.

## How does timonieris work?

<!TBD!> 
