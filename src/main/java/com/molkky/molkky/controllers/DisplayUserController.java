package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.UserDisplayModel;
import com.molkky.molkky.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import java.util.List;

@Controller
@RequestMapping("/user")
public class DisplayUserController {

    @Autowired
    UserRepository userRepository;

    @GetMapping("/displayUsers")
    public String displayUsers(Model model){
        UserDisplayModel userDisplayModel = new UserDisplayModel();
        model.addAttribute("users", userRepository.findAll());
        model.addAttribute("userDisplay", userDisplayModel);
        return "user/displayUsers";
    }

    @PostMapping("/searchUser")
    public String searchUser(Model model, @ModelAttribute("userDisplay")UserDisplayModel user){
        if(userRepository.existsUserByPseudo(user.getPseudo())){
            List<User> users;
            users = userRepository.findUsersByPseudo(user.getPseudo());
            model.addAttribute("users", users);
        }
        return "/user/displayUsers";
    }
}







