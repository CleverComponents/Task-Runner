# TaskRunner

<img align="left" src="images\task-runner-1-4.jpg"/>

This program serves for Software build automation, executing sequential tasks, including database backup/restore, running SQL scripts, Windows shell commands, Pascal scripts, passing variables through the whole task execution chain, and many more. You can set up a list of global parameters, such as Delphi application path, and use these parameters in tasks. You can even call a separated task chain from another task in the same way as you call Delphi procedure or a function, with passing parameters.



The first version of TaskRunner was created in 2002. The program UI was not modified for a long time. Now, we are working on improving the program, migrating to the latest version of RAD Studio, and replacing the user interface with modern controls and forms.
Initially, the program supported different scriping languages, including JavaScript, VBScript, and Delphi script. We used Dream Company scripting engine, which is not available anymore. The current version utilizes the PascalScripting library (by RemObjects) for running Pascal scripts. We are planning to implement JavaScript and VBScript, as well.



We hope, the publishing the TaskRunner sources on GitHub will help developers to automate their routine tasks, and save time on building and deploying their products.

You can contribute to the TaskRunner development by suggesting your fixes and improvements. Forks and pull requests are welcome.



Please feel free to star our repository to help other devs to learn about this project.

## Examples

* [Build InnoSetup Script](examples/BuildInnoSetupScript.job) - automates the compiling of an InnoSetup installation script, customizes the input parameters, and cleanups the output folder.
* [Build Delphi Project](examples/BuildDelphiProject.job) - tasks to compile a program using the Delphi command line compiler and pack using the pkzipc utility.   
Video tutorial on CleverComponents YouTube channel: [How to set up and run a project in TaskRunner](https://youtu.be/cndY-BVm8yA)
* [Test Scripter Task](examples/TestScripterTask.job) - a simple project, which shows how to run a Pascal script and pass parameters to this script.
