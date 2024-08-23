$(document).ready(function () {
  function adjustMappingTableHeight() {
    var table = document.getElementById("map_auxliary_fields-map_coralnet_labels-mapping_table");
    console.log("checking for table");

    if (table) {
      console.log("table exists");
      var wtHolder = table.getElementsByClassName("handsontable")[0].getElementsByClassName("wtHolder")[0];
      var newHeight = wtHolder.getElementsByClassName("wtHider")[0].offsetHeight;
      if (newHeight > 250) {
        var parent = document.getElementById("handsontable-parent");
        parent.style.overflowY = "auto";
        wtHolder.style.height = "500px";
      }

      clearInterval(intervalId);
    }
  };

  var intervalId = setInterval(adjustMappingTableHeight, 100);
});
