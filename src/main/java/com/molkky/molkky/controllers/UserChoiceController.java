package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.domain.UserTounamentRole;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTounamentRoleRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.ModelAndView;
import type.UserRole;

import javax.servlet.http.HttpSession;
import java.util.List;


@Controller
public class UserChoiceController {

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private UserTounamentRoleRepository userTounamentRoleRepository;

    @Autowired
    private TournamentRepository tournamentRepository;

    @GetMapping("/user_choice/choiceTournament")
    public String Index(HttpSession session, Model model){
        User user = (User)session.getAttribute("user_temp");
        List<Tournament> tournaments =  userTounamentRoleRepository.findTournamentFromUser(user);
        model.addAttribute("tournaments", tournaments);
        model.addAttribute("user", user);

        /*
        List<UserTounamentRole> adminOrStaff = userTounamentRoleRepository.findUserAdminStaff(user);
        //System.out.println(adminOrStaff.get(0).getRole());
        //System.out.println(adminOrStaff.get(1).getRole());
        model.addAttribute("listusers", adminOrStaff);

         */
        return "/user_choice/choiceTournament";
    }

    @PostMapping("/user_choice/choiceTournament")
    public ModelAndView submit(@RequestParam(value = "tournamentId") String tournamentId, ModelMap model, HttpSession session){
        Tournament tournament = tournamentRepository.findById(Integer.valueOf(tournamentId));
        User user = (User)session.getAttribute("user_temp");
        List<UserTounamentRole> roles = userTounamentRoleRepository.findUserTounamentRoleByTournamentAndUser(tournament,user);
        session.setAttribute("tournament",tournament);
        model.addAttribute("roles",roles);
        return new ModelAndView( "/user_choice/choiceRole", model) ;
    }

    @PostMapping("/user_choice/choiceRole")
    public ModelAndView choose(@RequestParam(value = "roleId") String roleId, HttpSession session, Model model){
        UserTounamentRole userTounamentRole = userTounamentRoleRepository.findById(Integer.valueOf(roleId));
        User userChoice= userTounamentRole.getUser();

        Tournament tournament = (Tournament) session.getAttribute("tournament");
        UserLogged userLogged = new UserLogged(userChoice.getEmail(), userChoice.getPassword(), userTounamentRole.getRole(),tournament);
        session.setAttribute("user",userLogged);
        return new ModelAndView( "/home") ;
    }


}
