// local-storage.js
// Browser localStorage integration for SPC App

// Save data to localStorage with app prefix
window.saveAppState = function(key, data) {
  try {
    console.log('Saving app state for key:', key);
    localStorage.setItem('spc_app_' + key, JSON.stringify(data));
    console.log('App state saved successfully');
    return true;
  } catch(e) {
    console.error('Failed to save to localStorage:', e);
    return false;
  }
};

// Load data from localStorage
window.loadAppState = function(key) {
  try {
    console.log('Loading app state for key:', key);
    var data = localStorage.getItem('spc_app_' + key);
    if (data) {
      console.log('App state loaded successfully');
      return JSON.parse(data);
    } else {
      console.log('No app state found for key:', key);
      return null;
    }
  } catch(e) {
    console.error('Failed to load from localStorage:', e);
    return null;
  }
};

// Clear specific key from localStorage
window.clearAppState = function(key) {
  try {
    console.log('Clearing app state for key:', key);
    localStorage.removeItem('spc_app_' + key);
    console.log('App state cleared successfully');
    return true;
  } catch(e) {
    console.error('Failed to clear localStorage:', e);
    return false;
  }
};

// Check if data exists in localStorage
window.hasAppState = function(key) {
  var exists = localStorage.getItem('spc_app_' + key) !== null;
  console.log('Checking app state for key:', key, 'exists:', exists);
  return exists;
};