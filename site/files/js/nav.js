$(document).ready(function(){
  /*$("div#nav li > ul").hide();*/
  /*$("div#nav li > ul.active").show();*/
  $("div#nav ul li a").click(function(event){
    // $(event.target).parent().children("ul").slideToggle("fast");
	if ($(event.target).html() == "User Manual »") document.location = "/MANUAL.html";
	else if ($(event.target).html() == "Wiki »") document.location = "/wiki";
  });
});
