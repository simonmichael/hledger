$(document).ready( function() {
  addDocVersions();
  highlightCurrentDocVersion();
});

function addDocVersions() {
  var parts = window.location.pathname.split('/');
  var page = parts.length > 0 ? parts[parts.length-1].slice(0,-5) : '';
  var hash = window.location.hash.slice(1);
  var topic = (page=='manual' && hash) ? hash : page;
  var newhash = (page=='manual' && topic!='manual') ? ('#'+topic) : '';
  var newpage = page=='manual' ? page : topic;
  $('.docversions').html('Available versions: \
<a href="/'+newpage+'.html'+(page=='manual' ? newhash : '')+'">dev</a> \
| <a href="/doc/1.12/'+newpage+'.html'+(page=='manual' ? newhash : '')+'">1.12</a> \
| <a href="/doc/1.11/'+newpage+'.html'+(page=='manual' ? newhash : '')+'">1.11</a> \
| <a href="/doc/1.10/'+newpage+'.html'+(page=='manual' ? newhash : '')+'">1.10</a> \
| <a href="/doc/1.9/'+newpage+'.html'+(page=='manual' ? newhash : '')+'">1.9</a> \
| <a href="/doc/1.5/'+newpage+'.html'+(page=='manual' ? newhash : '')+'">1.5</a> \
| <a href="/doc/1.4/'+newpage+'.html'+(page=='manual' ? newhash : '')+'">1.4</a> \
| <a href="/doc/1.3/'+newpage+'.html'+(page=='manual' ? newhash : '')+'">1.3</a> \
| <a href="/doc/1.2/'+newpage+'.html'+(page=='manual' ? newhash : '')+'">1.2</a> \
| <a href="/doc/1.1/'+newpage+'.html'+(page=='manual' ? newhash : '')+'">1.1</a> \
| <a href="/doc/1.0/'+newpage+'.html'+(page=='manual' ? newhash : '')+'">1.0</a> \
| <a href="/doc/0.27/manual.html'+(topic=='manual' ? '' : ('#'+topic))+'">0.27</a> \
');
}

function highlightCurrentDocVersion() {
  $('.docversions').each( function() {
    var parts = window.location.pathname.split('/');
    var dir = parts.length > 1 ? parts[parts.length-2] : '';
    var ver = $.isNumeric(dir) ? dir : 'dev';
    $(this).find('a').each( function() {
      if ($(this).html() == ver)
        $(this)
        // .removeAttr('href')
        // .css('text-decoration', 'none')
        // .css('color', 'initial')
        .css('font-weight','bold')
        // .hide()
        ;
    });
  });
}
