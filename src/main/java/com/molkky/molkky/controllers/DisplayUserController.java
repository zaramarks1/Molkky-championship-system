package com.molkky.molkky.controllers;

import com.molkky.molkky.controllers.superclass.DefaultAttributes;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleCustom;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

@Controller
@RequestMapping("/user")
public class DisplayUserController extends DefaultAttributes {

    @Autowired
    UserRepository userRepository;

    @Autowired
    TournamentRepository tournamentRepository;

    @Autowired
    UserTournamentRoleCustom userTournamentRoleCustom;

    @GetMapping("/displayUsers")
    public String displayUsers(Model model, @RequestParam(value = "filter", required = false) String filter){
        if(filter != null && !"".equals(filter)){
            model.addAttribute("users" , userRepository.searchUsersByName(filter, 10));
        } else {
            model.addAttribute("users" , userRepository.findAll());
        }
        return "user/displayUsers";
    }

    @GetMapping("/view")
    public String userView(Model model, @RequestParam(value = "userId")String userId){
        User user = userRepository.findById(Integer.valueOf(userId));
        model.addAttribute("user",user);
        model.addAttribute("tournament", userTournamentRoleCustom.findTournamentFromUser(user));
        return "/user/displayDetailsUser";
    }
}







