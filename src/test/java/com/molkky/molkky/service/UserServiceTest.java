package com.molkky.molkky.service;

import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.model.UserModel;
import com.molkky.molkky.repository.UserRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import type.UserRole;

@SpringBootTest
class UserServiceTest {
    @Autowired
    private UserService userService;
    @Autowired
    private UserRepository userRepository;
    @Test
    void createUserModelTest() {
//        given
        User user = new User();
        user.setEmail("bruh@gmail.com");
        user.setPseudo("bruhpseudo");
        user.setSurname("surname");
        user.setForename("forename");
        user.setClub("club");
//        when
        UserModel userModel = UserService.createUserModel(user);
//        then
        Assertions.assertEquals(user.getEmail(), userModel.getEmail());
        Assertions.assertEquals(user.getPseudo(), userModel.getPseudo());
        Assertions.assertEquals(user.getSurname(), userModel.getSurname());
        Assertions.assertEquals(user.getForename(), userModel.getForename());
        Assertions.assertEquals(user.getClub(), userModel.getClub());
    }
    @Test
    void createModelFromUserLoggedTest(){
//        given
        UserLogged userLogged = new UserLogged();
        userLogged.setEmail("mail");
        userLogged.setRole(UserRole.PLAYER);
//        when
        UserModel userModel = UserService.createUserModelFromUserLogged(userLogged);
//        then
        Assertions.assertEquals(userLogged.getEmail(), userModel.getEmail());
        Assertions.assertEquals(userLogged.getRole(), userModel.getRole());
        Assertions.assertEquals(userLogged.getId(), userModel.getId());
    }
    @Test
    void getUserFromModelTest(){
//        given
        User user = userRepository.save(new User());
        UserModel userModel = UserService.createUserModel(user);
//        when
        User userFromModel = userService.getUserFromModel(userModel);
//        then
        Assertions.assertEquals(user.getId(), userFromModel.getId());
    }
}
