# Project Template Repository

This repository is designed to serve as a template for the curation and maintenance of data within a specific project. Projects may items such as a thesis chapter or a manuscript publication. 

## Minimum metadata requirements

All repositories should also contain a `README.md` file which details the following metadata, as appropriate:

1. Project title
2. Lead HQP
3. Other HQP
4. Lead PI
5. Other PI
6. Data sources
7. Funding sources

## Project structure
All project repos are named according to the associated grant and labelled with a short informative tag for the project (e.g. grant_projectname'). In addition, they contain the following folders:

1. **data |** will contain separate folders for `raw_data`, `clean_data`, and `processed_data`, as well as data from databases incorporated as submodules. For more information related to reading in and using submodules, see `docs/using_submodules.md`.
2. **code |** scripts and other code for processing and analyzing project information
3. **docs |** project notes, etc.
4. **output |** saved outputs (e.g. figures and tables) from analysis


## Creating a new project for storage from the GitHub website

1. From the [template's website](https://github.com/biogeochem/project_template), click the _Use this template_ button.

2. Select the owner for your new repository. (This will be the `biogeochem` organization since you want it to be integrated within the organization. It would be you as an individual user *only* if you want to use this template for something outside of the lab group.)

3. Choose a name for your new repository in the format of `surname_projectname`.

4. Write a brief description about the repository.

5. Set the new repository to `Private` unless you want the contents to be made immediately public.

6. Click the _Create repository from template_ button.

7. Fork the new repository from the `biogeochem` organization to your user account, by clicking the _Fork_ button at the top right.

8. Clone the repo from your user account. After forking the repo you will be forward to your own fork. Click _Clone or download_, copy the URL and use software such as [GitKraken](https://www.gitkraken.com/) or [SourceTree](https://www.sourcetreeapp.com/). Alternatively, use the command line (`git clone …`).


## Naming and formatting conventions
All files should follow the standardized naming convention outlined below:

- Dates: YYYY-MM-DD
- Text: UTF-8 encoding
- File names: descriptive and searchable


## Repository use
Students or collaborators wishing to use data from a repo must abide by the following guidelines.

*For more information about project maintenance, please contact **Kateri Salk** at krsalkgu@uwaterloo.ca or **Megan Larsen** at mlarsen@wlu.ca or  **Jason Venkiteswaran** at jvenkiteswaran@wlu.ca.*

