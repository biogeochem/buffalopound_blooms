# Using Submodules for Cleaned Data
Historical databases are kept in their own repositories with limited access. Cleaned data that have undergone basic QA/QC are kept in separate repositories and can be added to projects as a `submodule`.

1. To add the clean data from the `database_test-clean_data` repository:

`git submodule add https://github.com/biogeochem/database_test-clean_data database_test-clean_data`

2. To update the submodule if there are upstream changes

`git submodule update --init --recursive`

3. To update the submodule to the newest version, ahead of where the original submodule was made:

`git submodule foreach --recursive 'git checkout master'`

*For more information about project maintenance, please contact **Megan Larsen** at mlarsen@wlu.ca or **Jason Venkiteswaran** at jvenkiteswaran@wlu.ca.*

