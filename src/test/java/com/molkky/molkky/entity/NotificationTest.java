package com.molkky.molkky.entity;

import Type.UserRole;
import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Notification;

import com.molkky.molkky.domain.User;
import com.molkky.molkky.repository.NotificationRepository;
import com.molkky.molkky.repository.UserRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.Rollback;

import javax.transaction.Transactional;
import java.util.HashSet;
import java.util.Set;

@SpringBootTest(classes = MolkkyApplication.class)
class NotificationTest {
    @Autowired
    private NotificationRepository notificationRepository;
    @Autowired
    private UserRepository userRepository;

    @Test
    @Transactional
    @Rollback(false)
    void testInsertNotification() {
        User user = userRepository.save(new User(
                "pseudo_test",
                "surname_test",
                "forename_test",
                "club_test",
                "email_test",
                true,
                UserRole.ADM

        ));
        Notification notification = notificationRepository.save(new Notification("test", "test", false, user));
        Set<Notification> notifs =  new HashSet<>();
        notifs.add(notification);
        user.setNotifications(notifs);

        Assertions.assertEquals("test", notification.getLink(), "Link is not correct");
        Notification recupNotif = notificationRepository.findById(notification.getId());
        Assertions.assertEquals("test", recupNotif.getLink(), "Link is not correct");

        Assertions.assertEquals(1, user.getNotifications().size(), "Number of notifs is not correct");
        User recupUser = userRepository.findById(user.getId());
        Assertions.assertEquals(1, recupUser.getNotifications().size(), "Number of notifs is not correct");
    }
}
