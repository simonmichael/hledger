$(document).ready(function(){
  $("div#nav li > ul").hide();
  $("div#nav li > ul.active").show();
  $("div#nav ul li a").click(function(event){
    $(event.target).parent().children("ul").slideToggle("fast");
  });
});
