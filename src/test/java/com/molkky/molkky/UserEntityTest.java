package com.molkky.molkky;

import com.molkky.molkky.domain.User;
import com.molkky.molkky.repository.UserRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest(classes = MolkkyApplication.class)
class UserEntityTest {
    @Autowired
    private UserRepository userRepository;

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
        Assertions.assertEquals("pseudo_test", user.getPseudo(), "Pseudo is not correct");
    }
}
