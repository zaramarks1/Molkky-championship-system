package com.molkky.molkky.controllers;

import com.molkky.molkky.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@RequestMapping("/user")
public class DisplayUserController {

    @Autowired
    UserRepository userRepository;

    @GetMapping("/displayUsers")
    public String displayUsers(Model model){
        model.addAttribute("users", userRepository.findAll());
        return "user/displayUsers";
    }
}







