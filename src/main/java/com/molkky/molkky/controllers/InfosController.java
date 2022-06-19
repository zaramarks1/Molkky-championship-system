package com.molkky.molkky.controllers;

import com.molkky.molkky.controllers.superclass.DefaultAttributes;

import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.domain.UserTournamentRole;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;

import javax.servlet.http.HttpSession;
import java.util.List;


@Controller
public class InfosController extends DefaultAttributes {


    @Autowired
    private MatchRepository matchRepository;

    @Autowired
    private UserTournamentRoleRepository userTournamentRoleRepository;

    @Autowired
    private TournamentRepository tournamentRepository;

    @Autowired
    private UserRepository userRepository;

    String redirect = "redirect:/infos";


    @GetMapping("/infos")
    public String index(Model model, HttpSession session){
        UserLogged userLogged = (UserLogged) session.getAttribute("user");
        User loggedUser = userRepository.findById(userLogged.getId());

        UserTournamentRole userTournamentRole = userTournamentRoleRepository.findUserTournamentRoleByUserId(loggedUser.getId());
        Tournament tournament = tournamentRepository.findById(userTournamentRole.getTournament().getId());

        String modifiedPseudo = loggedUser.getForename();
        if(loggedUser.getForename() == null) {
            modifiedPseudo = loggedUser.getForename()+"-X";
        }

        List<Match> matchList = matchRepository.findMatchAttributedToStaff(
                tournament,
                loggedUser
        );

        session.setAttribute("forename", modifiedPseudo);
        model.addAttribute(matchList);

        return "infos";
    }

    @PostMapping("/changePseudo")
    public String modifyPseudo(Model model,HttpSession session,@RequestParam(name="pseudo") String pseudo ){
        UserLogged user = getUser(session);
        User userDB = userRepository.findById(user.getId());
        userDB.setPseudo(pseudo);
        userRepository.save(userDB);
        user.setPseudo(pseudo);
        session.setAttribute("user",user);
        return redirect;
    }
    @PostMapping("/changeSurname")
    public String modifySurname(Model model,HttpSession session,@RequestParam(name="surname") String surname ){
        UserLogged user = getUser(session);
        User userDB = userRepository.findById(user.getId());
        userDB.setSurname(surname);
        userRepository.save(userDB);
        user.setSurname(surname);
        session.setAttribute("user",user);
        return redirect;
    }

    @PostMapping("/changeForename")
    public String modifyForename(Model model,HttpSession session,@RequestParam(name="forename") String forename ){
        UserLogged user = getUser(session);
        User userDB = userRepository.findById(user.getId());
        userDB.setForename(forename);
        userRepository.save(userDB);
        user.setForename(forename);
        session.setAttribute("user",user);
        return redirect;
    }

    @PostMapping("/changePassword")
    public String modifyPassword(Model model,HttpSession session,@RequestParam(name="pwd1") String pwd1, @RequestParam(name="pwd2") String pwd2 ){
        if(pwd1.equals(pwd2)) {
            UserLogged user = getUser(session);
            User userDB = userRepository.findById(user.getId());
            userDB.setPassword(pwd1);
            userRepository.save(userDB);
            user.setPassword(pwd2);
            session.setAttribute("user", user);
        }
        return redirect;
    }

    @PostMapping("/cancelRegisteration")
    public String deleteRegisteration(Model model,HttpSession session){
        UserLogged user = getUser(session);
        User userDB = userRepository.findById(user.getId());
        List<UserTournamentRole> userTournamentRoleDB = userTournamentRoleRepository.findUserTournamentRoleByTournamentAndUserAndRole(user.getTournament(),userDB, user.getRole());
        userTournamentRoleRepository.deleteAll(userTournamentRoleDB);
        return "redirect:/connexion";
    }


}
