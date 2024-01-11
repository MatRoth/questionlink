# Qlink

QuestionLink package for public usage. The package should enable researchers in the social sciences to make single item instruments from social science surveys comparable using Observed Score Equating in a Random Groups Design (OSE-RG).


## Description
The package should present a harmonization workflow following four steps:

    - Finding possibilites for harmonising data with OSE-RG
    - Providing an overview and sanity checks on the data provided
    - Creating harmonization solutions in the form of recoding tables that make single item survey instruments comparable
    - Harmonizing data by transforming it.    

![grafik-1.png](./grafik-1.png)


## Architecture

    - Keep expandability possible
    - What are the user facing functions? 
    - Which connections are allowed, which are not?
        - What are allowed direct connections?
        - What are allowed relay connections?
    - We only allow one relay (for now)

## Testing
    
    - Gold-Standard tests
    - Regression tests
    - Unit Tests

## Info on S3 oop
    - There are two object types in qlink: questionlink_prepare (product of ql_prepare()) and questionlink_harmonize (product of ql_harmonize())
    - Object types ("attributes") are added in the final step of the functions ql_prepare() and ql_harmonize() with a dedicated function.
    - S3 methods for those objects are found in S3_methods.R.
    - The S3 method for print.ql_prepare() is still missing. 
