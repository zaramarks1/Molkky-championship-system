package com.molkky.molkky.service;

import com.molkky.molkky.domain.Notification;
import com.molkky.molkky.domain.UserTournamentRole;
import com.molkky.molkky.repository.NotificationRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest
class NotificationServiceTest {
    @Autowired
    private NotificationRepository notificationRepository;
    @Autowired
    private NotificationService notificationService;
    @Autowired
    private UserTournamentRoleRepository userTournamentRepository;

    @Test
    void sendNotificationTest() {
//        given
        UserTournamentRole userTournamentRole = userTournamentRepository.save(new UserTournamentRole());
//        when
        for (int i = 0; i < 20; i++) {
            Notification notif = notificationService.sendNotification("lien vers google", "http://google.fr", userTournamentRole);
        }
//        then
        Assertions.assertEquals(20, userTournamentRole.getNotifications().size());
        Assertions.assertEquals("http://google.fr", userTournamentRole.getNotifications().get(0).getLink());
        Assertions.assertEquals("lien vers google", userTournamentRole.getNotifications().get(0).getMessage());
    }

    @Test
    void unreadNotificationTest() {
//        given
        UserTournamentRole userTournamentRole = userTournamentRepository.save(new UserTournamentRole());
//        when
        for(int i = 0; i < 2; i++) {
            Notification notif = notificationService.sendNotification(Integer.toString(i),"http://",  userTournamentRole);
            notificationService.markNotificationAsRead(notif);
        }
        for(int i = 0; i < 7; i++) {
            Notification notif = notificationService.sendNotification(Integer.toString(i),"http://",  userTournamentRole);
        }
//        then
        Assertions.assertEquals(7, notificationService.getUnreadNotificationCount(userTournamentRole));
    }

    @Test
    void markAllReadTest() {
//        given
        UserTournamentRole userTournamentRole = userTournamentRepository.save(new UserTournamentRole());
//        when
        for(int i = 0; i < 20; i++) {
            Notification notif = notificationService.sendNotification(Integer.toString(i),"http://",  userTournamentRole);
        }
        notificationService.markAllNotificationsAsRead(userTournamentRole);
//        then
        Assertions.assertEquals(0, notificationService.getUnreadNotificationCount(userTournamentRole));
    }
}
