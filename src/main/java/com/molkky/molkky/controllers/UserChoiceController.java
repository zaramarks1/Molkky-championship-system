package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.domain.UserTournamentRole;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.ModelAndView;

import javax.servlet.http.HttpSession;
import java.util.List;


@Controller
public class UserChoiceController {

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private UserTournamentRoleRepository userTournamentRoleRepository;

    @Autowired
    private TournamentRepository tournamentRepository;

    @GetMapping("/user_choice/choiceTournament")
    public String index(HttpSession session, Model model) {
        User user = (User) session.getAttribute("user_temp");
        List<Tournament> tournaments = userTournamentRoleRepository.findTournamentFromUser(user);
        model.addAttribute("tournaments", tournaments);
        model.addAttribute("user", user);

        return "/user_choice/choiceTournament";
    }

    @PostMapping("/user_choice/choiceTournament")
    public ModelAndView submit(@RequestParam(value = "tournamentId", required = false) String tournamentId, ModelMap model, HttpSession session, Model model2) {
        try {
            Tournament tournament = tournamentRepository.findById(Integer.valueOf(tournamentId));
            User user = (User) session.getAttribute("user_temp");
            List<UserTournamentRole> roles = userTournamentRoleRepository.findUserTounamentRoleByTournamentAndUser(tournament, user);
            session.setAttribute("tournament", tournament);
            model.addAttribute("roles", roles);
            return new ModelAndView("/user_choice/choiceRole", model);
        } catch (Exception e) {
            index(session, model2);
            return new ModelAndView("/user_choice/choiceTournament", model);
        }
    }

    @PostMapping("/user_choice/choiceRole")
    public ModelAndView choose(@RequestParam(value = "roleId", required = false) String roleId, HttpSession session, Model model){
        try {
            UserTournamentRole userTournamentRole = userTournamentRoleRepository.findById(Integer.valueOf(roleId));
            User userChoice = userTournamentRole.getUser();
            Tournament tournament = (Tournament) session.getAttribute("tournament");
            UserLogged userLogged = new UserLogged(userChoice.getId(),userTournamentRole.getId() ,userChoice.getEmail(), userChoice.getPassword(), userTournamentRole.getRole(), tournament);
            session.setAttribute("user", userLogged);
            return new ModelAndView("/home");
        }
        catch (Exception e){
            return new ModelAndView("/user_choice/choiceRole");
        }
    }


}
