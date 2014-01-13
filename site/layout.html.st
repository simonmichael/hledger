<html>
<head>
  <title>$pagetitle$ - $sitetitle$</title>
  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />

  <link rel="alternate" type="application/rss+xml" title="Joyful Systems blog, hledger tag" href="http://joyful.com/tags/hledger.xml" />

  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <link rel="stylesheet" href="//netdna.bootstrapcdn.com/bootstrap/3.0.3/css/bootstrap.min.css">
  <!--[if lt IE 9]>
   <script src="https://oss.maxcdn.com/libs/html5shiv/3.7.0/html5shiv.js"></script>
   <script src="https://oss.maxcdn.com/libs/respond.js/1.3.0/respond.min.js"></script>
  <![endif]-->

  <link rel="stylesheet" href="/css/style.css" />
  <!-- <link rel="stylesheet" type="text/css" media="print" href="$root$css/print.css" /> -->
  <style>
$if(home)$
h1 { font-size:7em; }
$endif$
  </style>

  <!-- <link rel="stylesheet" href="/js/highslide/highslide.css" /> -->
  <!-- <script src="/js/highslide/highslide.js"></script> -->
  <!-- <script> -->
  <!-- 	  hs.graphicsDir = '/js/highslide/graphics/'; -->
  <!-- 	  hs.outlineType = 'rounded-black'; -->
  <!-- 	  hs.captionEval = 'this.thumb.title'; -->
  <!-- </script> -->

  <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.3/jquery.min.js"></script>
  <script src="$root$js/nav.js"></script>

</head>
<body>

  <a href="https://github.com/simonmichael/hledger"><img style="position: absolute; top: 0; right: 0; border: 0;" src="https://s3.amazonaws.com/github/ribbons/forkme_right_green_007200.png" alt="Fork me on GitHub"></a>

  <div class="container">
    <div class="row">
	  <div id="nav" class="col-sm-2" style="margin-top:5em;">
$nav$
      </div>
	  <div class="col-sm-10">
$contents$
      </div>
    </div>
  </div>

  <div id="footer">
      <hr />
      &copy; 2007-2014 <a href="http://joyful.com/">Simon Michael</a> and contributors
  </div>

  <!-- <script src="js/bootstrap.min.js"></script> -->

  <script type="text/javascript">
      var gaJsHost = (('https:' == document.location.protocol) ? 'https://ssl.' : 'http://www.');
      document.write(unescape('%3Cscript src=\'' + gaJsHost + 'google-analytics.com/ga.js\' type=\'text/javascript\'%3E%3C/script%3E'));
  </script>
  <script type="text/javascript">
      try {
      var pageTracker = _gat._getTracker('UA-3456280-3');
      pageTracker._trackPageview();
      } catch(err) {}
  </script>

</body>
</html>
