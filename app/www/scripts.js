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

// Selectize sets autocomplete="new-password" on its search input to stop the
// browser's autofill, but password managers (Bitwarden etc.) then treat every
// dropdown as a new-password field and offer to generate one. Set it to "off"
// and add the password-manager ignore hints. Runs on load, whenever Shiny
// inserts new selectize inputs, and if selectize re-applies the attribute.
function fixSelectizeAutocomplete(root) {
  (root || document).querySelectorAll('.selectize-input input').forEach(function (inp) {
    if (inp.getAttribute('autocomplete') !== 'off') inp.setAttribute('autocomplete', 'off');
    if (!inp.hasAttribute('data-bwignore')) {
      inp.setAttribute('data-bwignore', 'true');   // Bitwarden
      inp.setAttribute('data-1p-ignore', 'true');  // 1Password
      inp.setAttribute('data-lpignore', 'true');   // LastPass
      inp.setAttribute('data-form-type', 'other'); // Dashlane
    }
  });
}

// Single-select selectize inputs normally ignore typing until the current
// selection is deleted with Backspace, and the caret sits on top of the end of
// the selected label. Instead, while the input is focused, hide the selected
// item and show its label as placeholder ("ghost") text, so typing starts a
// search right away. The actual value never changes until a new option is
// picked, so Shiny doesn't see an empty value; blurring without picking
// anything just restores the display.
function ghostSelectize(root) {
  if (!window.jQuery) return;
  (root || document).querySelectorAll('select.selectized').forEach(function (el) {
    var z = el.selectize;
    if (!z || z._wavGhost || z.settings.mode !== 'single') return;
    z._wavGhost = true;

    var $ctl = z.$control, $inp = z.$control_input;
    var allowTyping = false;

    // let keystrokes through even though the single slot is "full"
    var origIsFull = z.isFull;
    z.isFull = function () {
      return allowTyping ? false : origIsFull.apply(this, arguments);
    };
    // only for printable keys and paste: Enter etc. must still see the slot
    // as full, or the dropdown stays open after picking an option
    function wrap(name, fn) {
      var orig = z[name];
      z[name] = function (e) {
        if (fn && fn(e) === false) return;
        allowTyping = z.isFocused && (name === 'onPaste' || (e.key && e.key.length === 1));
        try { return orig.apply(this, arguments); } finally { allowTyping = false; }
      };
    }
    // "settled": an option was just picked. Focus stays in the box (so
    // keyboard users aren't dropped back to the top of the page), but it shows
    // the selection normally, without a caret, until the user types or reopens
    // the dropdown.
    var settled = false;
    function unsettle() {
      if (settled) { settled = false; update(); }
    }
    wrap('onKeyDown', function (e) {
      // Backspace/Delete in an empty search box would delete the selection
      // and send a blank value to Shiny; there's nothing to delete, so ignore
      if ((e.keyCode === 8 || e.keyCode === 46) && $inp.val() === '') {
        e.preventDefault();
        return false;
      }
      if (e.key && e.key.length === 1) unsettle();
    });
    wrap('onPaste', unsettle);

    function update() {
      var ghost = z.isFocused && z.items.length > 0 && !settled;
      var label = ghost ? $ctl.children('.item').first().text() : '';
      $ctl.toggleClass('wav-ghost', ghost);
      $ctl.toggleClass('wav-settled', z.isFocused && settled);
      if (ghost && $inp.val() === '') {
        $inp.attr('placeholder', label);
      } else if (z.settings.placeholder && !z.items.length) {
        $inp.attr('placeholder', z.settings.placeholder);
      } else {
        $inp.removeAttr('placeholder');
      }
      $inp.triggerHandler('update', { force: true }); // resize for placeholder
    }
    z.on('focus', function () { settled = false; update(); });
    z.on('blur', function () { settled = false; update(); });
    z.on('item_add', function () { settled = z.isFocused; update(); });
    z.on('dropdown_open', unsettle);
    z.on('type', update);
    $inp.on('input', update);
  });
}

document.addEventListener('DOMContentLoaded', function () {
  markNewTabLinks(document);
  labelMapControls(document);
  fixSelectizeAutocomplete(document);
  ghostSelectize(document);
  new MutationObserver(function (mutations) {
    mutations.forEach(function (m) {
      if (m.type === 'attributes') {
        if (m.target.closest && m.target.closest('.selectize-input')) {
          fixSelectizeAutocomplete(m.target.parentNode);
        }
        return;
      }
      m.addedNodes.forEach(function (n) {
        if (n.nodeType === 1) {
          markNewTabLinks(n.parentNode || n);
          labelMapControls(n.parentNode || n);
          fixSelectizeAutocomplete(n.parentNode || n);
          ghostSelectize(n.parentNode || n);
        }
      });
    });
  }).observe(document.body, {
    childList: true,
    subtree: true,
    attributes: true,
    attributeFilter: ['autocomplete']
  });
});
