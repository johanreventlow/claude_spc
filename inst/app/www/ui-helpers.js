// ui-helpers.js
// UI helper functions for SPC App

$(document).ready(function() {
  // Function to show UI when waiter is hidden
  window.showAppUI = function() {
    setTimeout(function() {
      $('body').css('opacity', '1');
    }, 100); // Small delay to ensure smooth transition
  };

  // Selectize dropup behavior configuration
  // Force dropup behavior for selectize inputs in .selectize-dropup containers
  $('.selectize-dropup').find('select').each(function() {
    if (this.selectize) {
      // Override the dropdown positioning
      var selectize = this.selectize;
      var originalSetup = selectize.setup;
      selectize.setup = function() {
        originalSetup.call(this);
        // Force dropdown to open upward
        this.$dropdown.addClass('dropup-forced');
      };
    }
  });
});