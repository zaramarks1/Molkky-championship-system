package com.molkky.molkky.service;

import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.model.UserModel;
import com.molkky.molkky.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class UserService {
    @Autowired
    private UserRepository userRepository;

    public static UserModel createUserModel(User user){
        UserModel userModel = new UserModel();
        userModel.setId(user.getId());
        userModel.setPseudo(user.getPseudo());
        userModel.setSurname(user.getSurname());
        userModel.setForename(user.getForename());
        userModel.setClub(user.getClub());
        userModel.setEmail(user.getEmail());
        return userModel;
    }

    public static UserModel createUserModelFromUserLogged(UserLogged userLogged){
        UserModel userModel = new UserModel();
        userModel.setId(userLogged.getId());
        userModel.setRole(userLogged.getRole());
        userModel.setEmail(userLogged.getEmail());
        return userModel;
    }

    public User getUserFromModel(UserModel userModel){
        return userRepository.findById(userModel.getId());
    }
}
