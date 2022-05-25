function getResults(event){
    var input = document.getElementById("searchbarInput");
    var autoResultsContainer = document.getElementById("searchAutoresults");
    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
        if (this.readyState === 4 && this.status === 200) {
            // Typical action to be performed when the document is ready:
            autoResultsContainer.innerHTML = xhttp.responseText;
            document.addEventListener('click', (e) => handleClick(e))

        }
    };
    xhttp.open("GET", "/search/search?term=" + input.value, true);
    xhttp.send();
}

function handleClick(e){
    if(!e.target.className.includes("popup")){
        document.getElementById("searchAutoresults").innerHTML = ""
        document.removeEventListener('click', (e) => handleClick(e))
    }
}
