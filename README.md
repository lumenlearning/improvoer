ImprovOER
=========

ImprovOER is a set of programs designed to facilitate improvement of course
content by providing instructional designers with various statistics and
indicators of the effectiveness of course content.

In its current state of development, ImprovOER focues primarily on student
assessment results and works only with the Canvas LMS.  Future versions will include analysis of content and provide functionality to interface with Blackboard.

Requirements
------------

Several prerequisites must be installed and/or configured before
running ImprovOER.

### Canvas API Prerequisites (Python)
 * A Python 3 interpreter
 * The canvaslms module (can be obtained from [GitHub](https://github.com/lumenlearning/python3-canvaslms-api))
 * The PYTHON_PATH environment variable (to indicate where the canvaslms module
   is located)

### Assessment Parameter Prerequisites (Clojure, Optional)
 * [Java](http://java.com/en/download/index.jsp) >= 1.5 (either JDK or JRE)
 * [Clojure](http://repo1.maven.org/maven2/org/clojure/clojure/1.5.1/clojure-1.5.1.zip) >= 1.5.1
 * The student and assessment parameter optimization program (will be added to the LumenLearning GitHub repository soon)

Example
-------
 1. Go to the ImprovOER directory.
 2. In the config directory, rename config.R.default to config.R (`mv config/config.R.default config/config.R`)
 3. Edit the configuration file (config/config.R) with the appropriate settings.
    (More documentation on how to do this is coming soon.)
 4. Run R from the root of the ImprovOER directory.
 5. Execute the following commands:

        > source("src/main.R")
        > create.crs.report(
            config=config,
            course.name="Name of Course",
            course.id=123456)
