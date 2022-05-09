package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.UserModel;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.service.EmailSenderService;
import com.molkky.molkky.service.RegisterService;
import com.molkky.molkky.service.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.servlet.ModelAndView;

@Controller
public class RegisterController {

    @Autowired
    private RegisterService registerService;
    @Autowired
    private UserService userService;
    @Autowired
    TournamentRepository tournamentRepository;
    @Autowired
    private EmailSenderService senderService;

    @GetMapping("/register")
    public String createUser(Model model) {
        User u = new User();
        model.addAttribute("user", u);
        return "register";
    }

    // TODO Retrieve the current tournament within the session
    @PostMapping("/saveUser")
    public ModelAndView saveUser(@ModelAttribute("user") UserModel userModel) {
        User user = userService.getUserFromModel(userModel);
        registerService.encodeAndSendEmail(user);
        registerService.saveUser(user);
        return new ModelAndView("redirect:/register");
    }
}
