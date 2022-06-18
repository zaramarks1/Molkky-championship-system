package com.molkky.molkky.controllers;

import com.molkky.molkky.controllers.superclass.DefaultAttributes;
import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.domain.UserTournamentRole;
import com.molkky.molkky.model.StaffForename;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.repository.MatchRepository;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;

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
        StaffForename staffForename = new StaffForename(modifiedPseudo);

        List<Match> matchList = matchRepository.findMatchAttributedToStaff(
                tournament,
                loggedUser
        );

        System.out.println(modifiedPseudo);
        session.setAttribute("forename", modifiedPseudo);
        model.addAttribute(matchList);

        return "infos";
    }
}
