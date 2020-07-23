# TaskRunner

<img align="left" src="Image\TaskRunner.jpg"/>

This program serves for Software build automation, executing sequential tasks, including database backup/restore, running SQL scripts, Windows shell commands, Pascal scripts, passing variables through the whole task execution chain, and many more. You can set up a list of global parameters, such as Delphi application path, and use these parameters in tasks. You can even call a separated task chain from another task in the same way as you call Delphi procedure or a function, with passing parameters.



The first version of TaskRunner was created in 2002. The program UI was not modified for a long time. Now, we are working on improving the program, migrating to the latest version of RAD Studio, and replacing the user interface with modern controls and forms.
Initially, the program supported different scriping languages, including JavaScript, VBScript, and Delphi script. We used Dream Company scripting engine, which is not available anymore. The current version utilizes the PascalScripting library (by RemObjects) for running Pascal scripts. We are planning to implement JavaScript and VBScript, as well.



We hope, the publishing the TaskRunner sources on GitHub will help developers to automate their routine tasks, and save time on building and deploying their products.

You can contribute to the TaskRunner development by suggesting your fixes and improvements. Forks and pull requests are welcome.



Please feel free to star our repository to help other devs to learn about this project.

The [GitHub/CleverComponents/Task-Runner](https://github.com/CleverComponents/Task-Runner) repository represents a list of code snippets and demo project. This list will be periodically updated, new versions of the project will be added.

