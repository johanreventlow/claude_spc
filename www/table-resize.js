// table-resize.js
// Dynamic rhandsontable height adjustment for both normal and fullscreen cards
// Based on GitHub issue: https://github.com/jrowen/rhandsontable/issues/355

$(document).ready(function() {
  console.log("Table resize script loaded");
  
  // Add CSS styles for dynamic table containers
  const style = document.createElement('style');
  style.textContent = `
    /* Normal mode - fixed container */
    .dynamic-table-container {
      height: 400px;
      min-height: 400px;
    }
    
    /* Fullscreen mode - flex layout */
    .card.bslib-full-screen .dynamic-table-container {
      flex: 1;
      min-height: 70vh;
      height: auto;
    }
    
    /* Ensure card body takes full height in fullscreen */
    .card.bslib-full-screen .card-body {
      height: calc(100vh - 120px) !important;
      display: flex !important;
      flex-direction: column !important;
    }
  `;
  document.head.appendChild(style);
  
  // Function to calculate optimal table height
  function calculateTableHeight(containerId) {
    const container = document.getElementById(containerId);
    if (!container) {
      console.log("Container not found:", containerId);
      return 400; // fallback
    }
    
    // Check if we're in fullscreen mode
    const card = container.closest('.card');
    const isFullscreen = card && card.classList.contains('bslib-full-screen');
    
    if (!isFullscreen) {
      console.log("Not in fullscreen, using default height 400px");
      return 400; // Use fixed height in normal mode
    }
    
    console.log("In fullscreen mode, calculating dynamic height");
    
    // Get the card body that contains the table
    const cardBody = container.closest('.card-body');
    if (!cardBody) {
      console.log("Card body not found for container:", containerId);
      return 400;
    }
    
    // Get card body dimensions
    const cardBodyRect = cardBody.getBoundingClientRect();
    const cardBodyHeight = cardBodyRect.height;
    
    // Calculate available space for table
    // Account for padding and other elements in card body
    const cardPadding = 20; // card body padding
    const otherElementsHeight = 50; // space for buttons, margins, etc.
    
    const availableHeight = cardBodyHeight - cardPadding - otherElementsHeight;
    
    // Set minimum and maximum bounds for fullscreen
    const minHeight = 400;
    const maxHeight = window.innerHeight * 0.8; // Max 80% of viewport height
    
    const calculatedHeight = Math.max(minHeight, Math.min(maxHeight, availableHeight));
    
    console.log("Height calculation for", containerId, ":", {
      isFullscreen: isFullscreen,
      cardBodyHeight: cardBodyHeight,
      availableHeight: availableHeight,
      calculatedHeight: calculatedHeight
    });
    
    return calculatedHeight;
  }
  
  // Function to resize the rhandsontable
  function resizeTable(containerId) {
    const container = document.getElementById(containerId);
    if (!container) {
      console.log("Container element not found:", containerId);
      return;
    }
    
    // Check current state
    const card = container.closest('.card');
    const isFullscreen = card && card.classList.contains('bslib-full-screen');
    console.log("Resize check - Container found:", !!container, "Card found:", !!card, "Is fullscreen:", isFullscreen);
    
    const newHeight = calculateTableHeight(containerId);
    
    // Get the Handsontable instance from the container
    const hotInstance = $(container).data('handsontable');
    if (!hotInstance) {
      console.log("Handsontable instance not found for:", containerId);
      return;
    }
    
    console.log("Resizing table", containerId, "to height:", newHeight, "(fullscreen:", isFullscreen, ")");
    
    // Update the table height
    hotInstance.updateSettings({
      height: newHeight
    });
    
    // Force a render to ensure the change takes effect
    hotInstance.render();
    
    console.log("Table resize completed");
  }
  
  // Debounce function to prevent excessive resize calls
  function debounce(func, wait) {
    let timeout;
    return function executedFunction(...args) {
      const later = function() {
        clearTimeout(timeout);
        func(...args);
      };
      clearTimeout(timeout);
      timeout = setTimeout(later, wait);
    };
  }
  
  // Debounced resize function
  const debouncedResize = debounce(function(containerId) {
    resizeTable(containerId);
  }, 100);
  
  // Monitor for table initialization and set up resize handling
  function setupTableResize(containerId) {
    console.log("Setting up resize for:", containerId);
    
    // Initial resize when table is ready
    setTimeout(function() {
      resizeTable(containerId);
    }, 500); // Wait a bit for table to initialize
    
    // Set up observers for various resize triggers
    
    // 1. Window resize
    $(window).on('resize', function() {
      debouncedResize(containerId);
    });
    
    // 2. Card fullscreen events - multiple approaches to catch bslib events
    
    // Method A: Listen for Bootstrap modal events
    $(document).on('shown.bs.modal', '.modal', function() {
      console.log("Bootstrap modal shown event detected");
      setTimeout(function() {
        debouncedResize(containerId);
      }, 100);
    });
    
    $(document).on('hidden.bs.modal', '.modal', function() {
      console.log("Bootstrap modal hidden event detected");
      setTimeout(function() {
        debouncedResize(containerId);
      }, 100);
    });
    
    // Method B: Direct click listener on fullscreen button
    $(document).on('click', '[data-bs-toggle="tooltip"][title*="full"]', function() {
      console.log("Fullscreen button clicked");
      setTimeout(function() {
        debouncedResize(containerId);
      }, 300); // Longer delay for DOM update
    });
    
    // Method C: Listen for any button clicks in card header (more generic)
    $(document).on('click', '.card-header button', function() {
      console.log("Card header button clicked, checking for fullscreen");
      setTimeout(function() {
        debouncedResize(containerId);
      }, 300);
    });
    
    // 3. MutationObserver for DOM changes (like fullscreen toggle)
    const observer = new MutationObserver(function(mutations) {
      let shouldResize = false;
      
      mutations.forEach(function(mutation) {
        if (mutation.type === 'attributes' && mutation.attributeName === 'class') {
          const target = mutation.target;
          const classList = target.classList;
          
          // Check if bslib-full-screen class was added or removed
          if (classList.contains('bslib-full-screen') || 
              mutation.oldValue && mutation.oldValue.includes('bslib-full-screen')) {
            console.log("Fullscreen class change detected on:", target);
            shouldResize = true;
          }
        }
      });
      
      if (shouldResize) {
        console.log("Fullscreen mutation detected, resizing table");
        debouncedResize(containerId);
      }
    });
    
    // Observe changes on the card and its ancestors
    const container = document.getElementById(containerId);
    if (container) {
      const card = container.closest('.card');
      if (card) {
        console.log("Setting up MutationObserver on card:", card);
        observer.observe(card, {
          attributes: true,
          attributeFilter: ['class'],
          attributeOldValue: true, // Important for detecting what changed
          subtree: false // Only observe the card itself, not children
        });
        
        // Also observe the document body for modal changes
        observer.observe(document.body, {
          attributes: true,
          attributeFilter: ['class'],
          subtree: true
        });
      }
    }
  }
  
  // Initialize when Shiny is ready
  $(document).on('shiny:sessioninitialized', function() {
    console.log("Shiny session initialized, setting up table resize");
    
    // Wait a bit more for the table to be fully rendered
    setTimeout(function() {
      // Target the main data table
      const mainTableId = 'main_data_table';
      if (document.getElementById(mainTableId)) {
        setupTableResize(mainTableId);
      } else {
        console.log("Main data table not found, will retry");
        // Retry after a longer delay
        setTimeout(function() {
          if (document.getElementById(mainTableId)) {
            setupTableResize(mainTableId);
          }
        }, 2000);
      }
    }, 1000);
  });
  
  // Also try to initialize immediately if Shiny is already ready
  if (typeof Shiny !== 'undefined' && Shiny.shinyapp && Shiny.shinyapp.isConnected()) {
    console.log("Shiny already connected, setting up table resize");
    setTimeout(function() {
      const mainTableId = 'main_data_table';
      if (document.getElementById(mainTableId)) {
        setupTableResize(mainTableId);
      }
    }, 1000);
  }
});