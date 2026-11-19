// https://stackoverflow.com/questions/31656689/how-to-save-img-to-users-local-computer-using-html2canvas

function saveAs(uri, filename) {
  var link = document.createElement('a');
  if (typeof link.download === 'string') {
    link.href = uri;
    link.download = filename;

    //Firefox requires the link to be in the body
    document.body.appendChild(link);

    //simulate click
    link.click();

    //remove the link when done
    document.body.removeChild(link);
  } else {
    window.open(uri);
  }
}

// add second top scrollbar for dt tables
// add callback = JS("addTopScroll(table);") to renderDataTable arguments
function addTopScroll(dtInstance) {
  var $container = $(dtInstance.table().container());
  var $scrollBody = $container.find('.dataTables_scrollBody');
  var $scrollHead = $container.find('.dataTables_scrollHead');

  if ($container.find('.top-scroll-wrapper').length > 0) {
    return;
  }

  // 1. Create the top scrollbar container (initially hidden)
  var $topScroll = $('<div>').addClass('top-scroll-wrapper').css({
    'width': '100%',
    'overflow-x': 'scroll',
    'overflow-y': 'hidden',
    'height': '15px',
    'display': 'none' // Start hidden until we verify overflow
  });

  // 2. Create inner div to match content width
  var $scrollContent = $('<div>').html('&nbsp;');
  $topScroll.append($scrollContent);

  // 3. Insert before header
  $scrollHead.before($topScroll);

  // 4. Logic to toggle visibility based on overflow
  function updateScrollVisibility() {
    var scrollWidth = $scrollBody[0].scrollWidth;
    var clientWidth = $scrollBody[0].clientWidth;
    
    // Update the inner width to match the table
    $scrollContent.width(scrollWidth);

    // Only show if content is wider than the visible area
    // We use a small buffer (e.g., 2px) to prevent rounding errors flickering
    if (scrollWidth > clientWidth + 2) {
      $topScroll.show();
    } else {
      $topScroll.hide();
    }
  }

  // 5. Sync scrolling
  $topScroll.on('scroll', function() {
    $scrollBody.scrollLeft($(this).scrollLeft());
  });
  
  $scrollBody.on('scroll', function() {
    $topScroll.scrollLeft($(this).scrollLeft());
  });

  // 6. Event Listeners for Resize
  $(window).on('resize', function() {
    updateScrollVisibility();
  });

  // Run once immediately to set initial state
  updateScrollVisibility();
}


// --- Accessibility helpers ---------------------------------------------------

// Announce a message to screen readers via the polite live region in ui.R
function announce(msg) {
  var el = document.getElementById('sr-announcer');
  if (!el) return;
  el.textContent = '';
  // small delay so repeated identical messages are still announced
  setTimeout(function () { el.textContent = msg; }, 100);
}

if (window.Shiny) {
  Shiny.addCustomMessageHandler('announce', announce);
}

// Append a visually-hidden "(opens in new tab)" to links that open a new
// window, so screen reader users are warned (WCAG 3.2.5 / G201). Runs on
// load and whenever Shiny inserts new content.
function markNewTabLinks(root) {
  (root || document).querySelectorAll('a[target="_blank"]:not([data-newtab])').forEach(function (a) {
    a.setAttribute('data-newtab', '1');
    if (!a.getAttribute('rel')) a.setAttribute('rel', 'noopener');
    var s = document.createElement('span');
    s.className = 'visually-hidden';
    s.textContent = ' (opens in new tab)';
    a.appendChild(s);
  });
}

// Leaflet's icon-only controls only have a title tooltip (or nothing);
// give them accessible names. Re-run on DOM changes because the app re-adds
// the layers control after load.
function labelMapControls(root) {
  (root || document).querySelectorAll('.easy-button-button:not([aria-label])').forEach(function (b) {
    if (b.title) b.setAttribute('aria-label', b.title);
    if (b.id === '[object Object]') b.removeAttribute('id');
  });
  (root || document).querySelectorAll('.leaflet-control-layers-toggle:not([aria-label])').forEach(function (t) {
    t.setAttribute('aria-label', 'Map layers and basemaps');
    t.setAttribute('role', 'button');
  });
}

document.addEventListener('DOMContentLoaded', function () {
  markNewTabLinks(document);
  labelMapControls(document);
  new MutationObserver(function (mutations) {
    mutations.forEach(function (m) {
      m.addedNodes.forEach(function (n) {
        if (n.nodeType === 1) {
          markNewTabLinks(n.parentNode || n);
          labelMapControls(n.parentNode || n);
        }
      });
    });
  }).observe(document.body, { childList: true, subtree: true });
});
