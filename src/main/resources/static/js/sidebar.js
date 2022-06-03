function openNav() {
    document.getElementsByClassName("sidebarContainerMobile")[0].style.left = "0px";
    document.getElementsByClassName("bgSidebarOpen")[0].style.display = "block";
    document.addEventListener('click', (e) => clickOutsideCloseNav(e));
}
function closeNav(){
    document.getElementsByClassName("sidebarContainerMobile")[0].style.left = "-250px";
    document.getElementsByClassName("bgSidebarOpen")[0].style.display = "none";
}

function clickOutsideCloseNav(e){
    if(e.target.className.includes("bgSidebarOpen")&&!e.target.className.includes("hamburgerIcon")){
        closeNav();
        document.removeEventListener('click', (e) => clickOutsideCloseNav(e));
    }
}