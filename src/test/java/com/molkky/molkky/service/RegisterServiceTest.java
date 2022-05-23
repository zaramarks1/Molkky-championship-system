package com.molkky.molkky.service;

import com.molkky.molkky.MolkkyApplication;

import com.molkky.molkky.domain.User;
import com.molkky.molkky.repository.UserRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.Rollback;

import javax.transaction.Transactional;

@SpringBootTest(classes = MolkkyApplication.class)
public class RegisterServiceTest {

    @Autowired
    RegisterService registerService;



    @Test
    @Rollback(false)
    @Transactional
    void saveUser(){
        User u = new User();
        u.setSurname("test register");

        u = registerService.saveUser(u);

        Assertions.assertEquals(u.getSurname(), "test register", "wrong name");
        Assertions.assertNotNull(u, "user not created");


    }
}
