package com.molkky.molkky;

import com.molkky.molkky.domain.User;
import com.molkky.molkky.repository.UserRepository;
import org.junit.Assert;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.TestComponent;

@SpringBootTest(classes = MolkkyApplication.class)
public class UserEntityTest {
    @Autowired
    private UserRepository userRepository;

    @Test
    void testInsertUser(){
        User user = userRepository.save(new User("Pierre","Pierre","Menanteau","eseo","pm@",false));
        User recupUser = userRepository.findById(user.getId());
        Assertions.assertEquals("Pierre", recupUser.getPseudo(), "User name is not corrected");
        Assertions.assertEquals("Pierre", recupUser.getSurname());
        Assertions.assertEquals("Menanteau", recupUser.getForename());
        Assertions.assertEquals("eseo", recupUser.getClub());
        Assertions.assertEquals("pm@", recupUser.getEmail());
        Assertions.assertEquals("Pierre", recupUser.getPseudo());
    }
}
