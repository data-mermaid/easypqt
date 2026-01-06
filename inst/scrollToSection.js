$(document).ready(function () {
  function scrollToSection() {
    var section = $('$$$id$$$');
    console.log("checking for section");

    if (section) {
      $('html, body').animate({ scrollTop: $(section).offset().top}, 'smooth');

      clearInterval(intervalId);
    }
  };

  var intervalId = setInterval(scrollToSection, 100);
});
