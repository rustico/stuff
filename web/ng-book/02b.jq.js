$.fn.clock = function() {
  function updateClock() {
    that.text(new Date());
  }

  setInterval(function() {
      updateClock();
  }, 1000);

  var that = this;
  updateClock();
};
