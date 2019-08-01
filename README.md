 <h1>argodc - simple package documentation library for common lisp</h1><br><br>
 
 Dependencies are "alexandria" and "cl-ppcre". <br><br>
 This library is thread safe and enables you to log in threads.<br><br>
 To load and use this library, cd into the "src/" directory, start your lisp (testet on sbcl, ecl, ccl)
 and execute:
 
	 (asdf:load-system :argparse)
   
To create arparse-package documentation:<br><br>

	 (asdf:load-system :argparse-app)
   (main)
   
To create a package documentation for a given package xxx:<br><br>

	 (argdoc:document :package :xxx
                    :filename "xxx.html"
                    :path "../doc/"
                    :doc-type 'argdoc:doc-html)
                    
When you create a html-file, then copy the css-file in ../doc to your ouput directory.<br><br>
