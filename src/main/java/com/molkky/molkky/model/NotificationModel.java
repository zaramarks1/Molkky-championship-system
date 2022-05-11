package com.molkky.molkky.model;

import com.molkky.molkky.domain.Notification;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;

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
    public static List<NotificationModel> createTeamModels(List<Notification> notifications) {
        List<NotificationModel> notificationModels = new ArrayList<>();
        for (Notification notification : notifications) {
            if(notification.getId() != null) {
                notificationModels.add(new NotificationModel(notification));
            }
        }
        return notificationModels;
    }

}
