// shiny-handlers.js
// Custom Shiny message handlers for SPC App

// Handler for saving app state via Shiny messages
Shiny.addCustomMessageHandler('saveAppState', function(message) {
  console.log('Received saveAppState message:', message);
  var success = window.saveAppState(message.key, message.data);
  if (!success) {
    console.error('saveAppState failed for key:', message.key);
  }
});

// Handler for loading app state via Shiny messages
Shiny.addCustomMessageHandler('loadAppState', function(message) {
  console.log('Received loadAppState message:', message);
  var data = window.loadAppState(message.key);
  Shiny.setInputValue('loaded_app_state', data, {priority: 'event'});
});

// Handler for clearing app state via Shiny messages
Shiny.addCustomMessageHandler('clearAppState', function(message) {
  console.log('Received clearAppState message:', message);
  var success = window.clearAppState(message.key);
  if (!success) {
    console.error('clearAppState failed for key:', message.key);
  }
});

// Handler for showing app UI after loading
Shiny.addCustomMessageHandler('showAppUI', function(message) {
  window.showAppUI();
});

// Auto-load existing session data on app start
$(document).ready(function() {
  if (window.hasAppState('current_session')) {
    setTimeout(function() {
      console.log('Auto-loading saved session data...');
      var data = window.loadAppState('current_session');
      if (data) {
        console.log('Found saved session data, triggering auto-restore');
        Shiny.setInputValue('auto_restore_data', data, {priority: 'event'});
      }
    }, 500); // Wait for Shiny to be ready
  }
});