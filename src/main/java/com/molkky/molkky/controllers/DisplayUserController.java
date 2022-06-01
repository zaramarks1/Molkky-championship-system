package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.UserDisplayModel;
import com.molkky.molkky.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.Banner;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

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
            model.addAttribute("users", userRepository.findUsersByPseudo(user.getPseudo()));
        }
        return "/user/displayUsers";
    }

    @GetMapping("/view")
    public String userView(Model model, @RequestParam(value = "userId")String userId){
        User user = userRepository.findById(Integer.valueOf(userId));
        model.addAttribute("pseudo", user.getPseudo());
        model.addAttribute("userId", userId);
        return "/user/displayDetailsUser";
    }
}







