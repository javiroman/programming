*** Settings ***
Documentation  This is basic info about the whole suite
Library  SeleniumLibrary


*** Variables ***


*** Test Cases ***
User must sign in to check out
    [Documentation]  This is basic info about the test
    [Tags]  Smoke
    open browser  http://www.amazon.es  googlechrome
    wait until page contains  Entra ya
    input text  id=twotabsearchtextbox  Ferrari 458
    click button  value=Ir
    close browser

*** Keywords ***
