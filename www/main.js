

function myclick(d) {
  Shiny.onInputChange("node", d.name);
}

$(document).keyup(function(event) {
    if (event.keyCode == 13) {
        $("#go").click();
    }
});