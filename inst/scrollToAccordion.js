$(document).ready(function () {
  function scrollToAccordion() {
    var section = $('.accordion-item[data-value=$$$id$$$]');
    console.log("checking for accordion");

    if (section) {
      $('html, body').animate({ scrollTop: $(section).offset().top}, 'smooth');

      clearInterval(intervalId);
    }
  };

  var intervalId = setInterval(scrollToAccordion, 100);
});
