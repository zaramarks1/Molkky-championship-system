function getResults(){
    var input = document.getElementById("searchbarInput").value;
    var autoResultsContainer = document.getElementById("searchAutoresults");
    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
        if (this.readyState === 4 && this.status === 200) {
            // Typical action to be performed when the document is ready:
            autoResultsContainer.innerHTML = xhttp.responseText;
        }
    };
    xhttp.open("GET", "/search/search?term=" + input, true);
    xhttp.send();
}