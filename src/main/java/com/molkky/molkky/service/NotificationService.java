package com.molkky.molkky.service;

import com.molkky.molkky.domain.Notification;
import com.molkky.molkky.domain.UserTournamentRole;
import com.molkky.molkky.model.NotificationModel;
import com.molkky.molkky.repository.NotificationRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class NotificationService {
    @Autowired
    private NotificationRepository notificationRepository;
    @Autowired
    private UserTournamentRoleRepository userTournamentRoleRepository;

    public Notification sendNotification(String message, String link, UserTournamentRole userTournamentRole) {
        Notification notification = new Notification();
        notification.setLink(link);
        notification.setMessage(message);
        notification.setUserTournamentRole(userTournamentRole);
        notification = notificationRepository.save(notification);
        userTournamentRole.getNotifications().add(notification);
        userTournamentRoleRepository.save(userTournamentRole);
        return notification;
    }

    public Integer getUnreadNotificationCount(UserTournamentRole userTournamentRole) {
        return Math.toIntExact(userTournamentRole.getNotifications().stream().filter(notification -> !notification.isRead()).count());
    }

    public void markNotificationAsRead(Notification notification) {
        notification.setRead(true);
        notificationRepository.save(notification);
    }

    public void markAllNotificationsAsRead(UserTournamentRole userTournamentRole) {
        userTournamentRole.getNotifications().forEach(this::markNotificationAsRead);
    }

    public List<NotificationModel> getNotificationModels(UserTournamentRole userTournamentRole) {
        List<Notification> notifications = userTournamentRole.getNotifications();
        return NotificationModel.createTeamModels(notifications);
    }

    public void sendNotificationToList(String message, String link, List<UserTournamentRole> listUserTournamentRole){
        for(UserTournamentRole user : listUserTournamentRole){
            sendNotification(message,link,user);
        }
    }


}
