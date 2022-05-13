# Contributing to [project name]

## Overview
Please note our general guidelines for contributing to NDCLab projects [here](https://ndclab.github.io/wiki/docs/contributing.html).

* [Roadmap](#Roadmap)  
* [Directory Structure](#Directory-Structure)  
* [Scripts](#Scripts)
* [Containers](#Containers)  
* [Workflow](#Workflow)  


## Roadmap
Please see the roadmap available on the [README.md](README.md) file of this repository.

:point_right: Keep the "Roadmap" text above but update the link to the `readme` for your repo. And, of course, delete this note before publishing the contributing file.


## Directory Structure
:point_right: Update this section as appropriate for your repo, then delete this note before publishing.

```yml
project-name
├── code
├── containers
├── CONTRIBUTING.md
├── data-monitoring
    ├──data-monitoring-log.md
    ├──data-monitoring-protocol.md
    ├──postprocessing-check
    ├──results-check
├── derivatives
    ├──preprocessed
├── literature
├── materials
    ├──questionnaires
    ├──task1
          ├──protocol.md
          ├──scripts
          ├──stimuli
├── notebook.md
├── README.md
├── sourcedata
    ├──checked
    ├──raw
```

### Container
To ensure reproducibility of results and software, a default docker file and singularity container are included with this template repository. The respective [README.md](README.md) contains a comprehensive guide on how to get started with the dockerfile. A step-by-step guide to getting started also included in the following [video](https://www.youtube.com/watch?v=oO8n3y23b6M). 


## Workflow
Workflow for both internal and external lab members is outlined on the [NDCLab contributing wiki page](https://ndclab.github.io/wiki/docs/contributing.html). 
