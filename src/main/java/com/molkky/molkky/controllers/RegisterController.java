package com.molkky.molkky.controllers;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.service.EmailSenderService;
import com.molkky.molkky.service.RegisterService;
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
    TournamentRepository tournamentRepository;
    @Autowired
    private EmailSenderService senderService;

    @GetMapping("/register")
    public String createUser(Model model) {
        User u = new User();
        model.addAttribute("user", u);
        return "register";
    }
    /*public String Home(Model model){
        System.out.print("SHEEESH2");
        List<User> listUsers = registerService.getAllUsers();
        model.addAttribute("listUsers", listUsers);
        return "/register";
    }*/


    // TO DO Retrieve the current tournament within the session
    @PostMapping("/register")
    public ModelAndView saveUser(@ModelAttribute("user") User user) {
        user.setTournament(tournamentRepository.findById(1));
        registerService.encodeAndSendEmail(user);
        registerService.saveUser(user);
        return new ModelAndView("redirect:/connexion");
    }
}
