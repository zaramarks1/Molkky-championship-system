function toggleNotifications(){
    var notifications = document.getElementById("notificationList");

    if(notifications.style.display === "block"){
        notifications.style.display = "none";
    } else{
        markAllAsRead();
    }

}

function markAllAsRead(){
    var xhr = new XMLHttpRequest();
    xhr.open("POST", "/notifications/markAllAsRead", true);
    xhr.setRequestHeader('Content-Type', 'application/json');
    xhr.send();
    xhr.onreadystatechange = function () {
        if (this.readyState !== 4) return;

        if (this.status === 200) {
            hideUnreadCount();
        }
    };
}

function hideUnreadCount(){
    document.getElementById("notificationList").style.display = "block";
    var unreadCount = document.getElementById("unreadCount");
    unreadCount.style.display = "none";
}