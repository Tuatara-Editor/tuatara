// Filter the contents column of the manual to the headings holding what is
// typed in its box. No address is ever loaded and nothing is stored.
(function () {
  var nav = document.querySelector("nav.toc");
  var box = nav && nav.querySelector("input");
  if (!box) { return; }
  box.addEventListener("input", function () {
    var words = box.value.trim().toLowerCase();
    nav.classList.toggle("filtering", words !== "");
    nav.querySelectorAll("li").forEach(function (item) {
      var hit = words === "" || item.textContent.toLowerCase().indexOf(words) !== -1;
      item.classList.toggle("hidden", !hit);
    });
  });
}());
