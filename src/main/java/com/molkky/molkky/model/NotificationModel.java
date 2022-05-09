package com.molkky.molkky.model;

import com.molkky.molkky.domain.Notification;
import lombok.Data;

@Data
public class NotificationModel {
    private Integer id;
    private String link;
    private String message;
    private boolean read;

    public NotificationModel(Notification notification) {
        this.id = notification.getId();
        this.link = notification.getLink();
        this.message = notification.getMessage();
        this.read = notification.isRead();
    }
}
