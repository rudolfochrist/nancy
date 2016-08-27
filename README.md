# NAME

nancy &#x2014; A (very) thin web wrapper

# VERSION

    Version 0.1.0

# SYNOPSIS

    (defpackage #:nancy-demo
      (:use :cl :nancy))
    (in-package :nancy-demo)
    
    (xget ("/")
      "Finally at home.")
    
    (start :server :hunchentoot
           :port 4242)

# DESCRIPTION

`nancy` is (another) [Sinatra](http://www.sinatrarb.com/)-like web framework. Actually `nancy` is a (very) thin wrapper around
[fukamachi/ningle](https://github.com/fukamachi/ningle). 

`nancy` is wholly unopinionated and should be orthogonal to your stack. It neither prescribes HTML rendering
or DB drivers nor does it prescribe a fixed directory structure. Use whatever you like most! 

## FUNCTIONS

### xget

`xget` defines a handler for a GET request. 

    (xget ("/greeting")
      "Hi")

To use path variables, specify them with a trailing colon:

    (xget ("/greet/:name")
      (let ((name (params :name)))
        (format nil "Hi ~A" name)))

You can use wildcards and regular expressions in URL, just like in ningle. 

For other HTTP methods use `xpost`, `xput`, `xpatch`, `xdelete` and `xoptions` respectively.

### params

You can access request parameters through the `params` function. To access path variables pass the same keyword
you used in the path to `params`:

    (xget ("/users/:id")
      (let ((id (params :id)))
        (format nil "User with ID=~A requested" id)))

If you want to access GET parameters (or POST) you need to use a string with the parameter name: 

    ;;; URL: /greet-me?name=Frank
    (xget ("/greet-me")
      (let ((name (params "name")))
        (format nil "Hi ~A" name)))

### redirect

As the name indicates this function can be used to establish redirects.

    (xpost ("/users")
      (let ((username (params "username"))
            (email (params "email")))
        (create-user name email)
        (redirect "/")))

If you want to use a different redirect status you can pass it along to `redirect` either as status number or
as keyword.

    (xget ("/old-endpoint")
      (redirect "/new-endpoint" :status 301))

is the same as 

    (xget ("/old-endpoint")
      (redirect "/new-endpoint" :status :moved-permanently))

### start

This starts up the server. You can pass the port or the server has keyword arguments. 

    (start :port 4242 :server :hunchentoot)

Since `nancy` is based on [ningle](https://github.com/fukamachi/ningle) and [ningle](https://github.com/fukamachi/ningle) is based on [clack](http://clacklisp.org/) you can switch the web server easily:

    (ql:quickload :woo)
    (start :port 4242 :server :woo)

### stop

If the times has come to put your webapp to rest you can use `stop` to gracefully stop it.

### status

`status` is a helper function to set the response status easily. Like `redirect` it takes a numeric or a keyword
status code. For example.

    (xpost ("/users")
      (let ((username (params "username"))
            (email (params "email")))
        (create-user useranme email)
        (status :created)))

## VARIABLES

### CONFIGURATION VALUES

-   **`*static-root*`:** Set `*static-root*` to the directory path of your static files (e.g. Javascript or CSS) are
    located. This defaults to `static/`.
-   **`*static-path*`:** Set `*static-path*` to the URL endpoint you like to use. This defaults to `/public/`.

### OTHER VARIABLES

-   **`*request*`:** The current request object.
-   **`*response*`:** The current response object.
-   **`*webapp*`:** The current `ningle` instance. This shouldn't be modified directly.

# LIMITATIONS

Although `nancy` *works*<sup><a id="fnr.1" class="footref" href="#fn.1">1</a></sup> on multiple Common Lisp implementations, it favours [SBCL](http://sbcl.org/). This means, if you use a
different implementation you can run into un-addressed bugs and limitations. 

# AUTHOR

Sebastian Christ (<rudolfo.christ@gmail.com>)

# COPYRIGHT

Copyright (c) 2016 Sebastian Christ (rudolfo.christ@gmail.com)

Released under the LLGPL license.

# SEE ALSO

<div id="footnotes">
<h2 class="footnotes">Footnotes: </h2>
<div id="text-footnotes">

<div class="footdef"><sup><a id="fn.1" class="footnum" href="#fnr.1">1</a></sup> <div class="footpara">And with works we mean compiles.</div></div>


</div>
</div>
