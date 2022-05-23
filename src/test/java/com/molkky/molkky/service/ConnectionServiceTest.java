package com.molkky.molkky.service;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.User;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.test.annotation.Rollback;

import javax.transaction.Transactional;

@SpringBootTest(classes = MolkkyApplication.class)

public class ConnectionServiceTest {

    @Autowired
    ConnexionService connexionService;

    @Test
    @Rollback(false)
    @Transactional
    void decodeTest(){

        User u = new User();
        u.setSurname("test decode");

        PasswordEncoder passwordEncoder = new BCryptPasswordEncoder();
        String hashedPassword = passwordEncoder.encode("password");
        u.setPassword(hashedPassword);

        boolean isGood = connexionService.decode("password",u );

        Assertions.assertTrue(isGood);

        boolean isNotGood = connexionService.decode("wrongPassword",u );

        Assertions.assertFalse(isNotGood);


    }
}
