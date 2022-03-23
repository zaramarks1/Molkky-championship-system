package com.molkky.molkky;

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
class UserEntityTest {
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private NotificationRepository notificationRepository;

    @Test
    void testInsertUser() {
        User user = userRepository.save(new User(
                "pseudo_test",
                "surname_test",
                "forename_test",
                "club_test",
                "email_test",
                true
        ));
        Assertions.assertEquals("pseudo_test", user.getPseudo(), "Pseudo is not correct");
        User recupUser = userRepository.findById(user.getId());
        Assertions.assertEquals("pseudo_test", recupUser.getPseudo(), "Pseudo is not correct");
    }

    @Test
    @Transactional
    @Rollback(false)
    void testInsertUserNotification() {
        User user = userRepository.save(new User(
                "pseudo_test",
                "surname_test",
                "forename_test",
                "club_test",
                "email_test",
                true
        ));
        Notification notification = notificationRepository.save(new Notification("test", "test", false, user));
        Set<Notification> notifs =  new HashSet<>();
        notifs.add(notification);
        user.setNotifications(notifs);
        Assertions.assertEquals(1, user.getNotifications().size(), "Number of notifs is not correct");
        User recupUser = userRepository.findById(user.getId());
        Assertions.assertEquals(1, recupUser.getNotifications().size(), "Number of notifs is not correct");
    }
}
