# fish550-2025

GitHub Repo for FISH 550 2025 https://atsa-es.github.io/atsa/

## Labs

Look in the ReadMe file in the Lab folder. Do your team work in your team folder. Once done, label your final team write-up as `Lab-X-team-X_final.Rmd`.

* [Lab 1](https://github.com/atsa-es/fish550-2025/tree/main/Lab-1) 
* [Lab 2](https://github.com/atsa-es/fish550-2025/tree/main/Lab-2)
* [Lab 3](https://github.com/atsa-es/fish550-2025/tree/main/Lab-3) 
* [Lab 4](https://github.com/atsa-es/fish550-2025/tree/main/Lab-4) 
* [Lab 5](https://github.com/atsa-es/fish550-2025/tree/main/Lab-5)

## Building the lab book (for Eli)

### Prerequisites

1. Make sure GH Pages is set up to use gh-pages branch

<img width="431" alt="image" src="https://github.com/user-attachments/assets/343f1a70-b5c4-47d8-aa14-764d5a6bc681" />

2. Make sure the gh-pages branch exists. You can create on GH
   
<img width="323" alt="image" src="https://github.com/user-attachments/assets/49b7fbc1-62c0-4b7a-bd71-6e2a306c962d" />

4. Back on RStudio, make sure you are not on the GH branch (locally). Do a Git pull to make sure you have the `gh-pages` branch locally but don't switch to it.

### To update the lab book

Open a terminal and make sure you are in fish550-2025. Run this
```
quarto publish gh-pages
```
The first time this is run, it will fail. Do a manual push of the gh-pages branch. After that, the publish command will work.
