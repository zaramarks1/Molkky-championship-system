package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.domain.UserTournamentRole;

import com.molkky.molkky.model.AddStaff;
import com.molkky.molkky.model.AddStaffList;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import org.apache.commons.lang.RandomStringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import type.UserRole;

import java.util.ArrayList;
import java.util.List;


@Controller
@RequestMapping("/staff")
public class StaffController {

    @Autowired
    UserRepository userRepository;

    @Autowired
    TournamentRepository tournamentRepository;

    @Autowired
    UserTournamentRoleRepository userTournamentRoleRepository;

    @PostMapping("/add")
    public String addStaff(@ModelAttribute("staff") AddStaffList staff){

        Tournament tournament = tournamentRepository.findById(staff.getMails().get(0).getTournamentId());
        List<UserTournamentRole> userTournamentRoles = new ArrayList<>();

        for(AddStaff s: staff.getMails()){
            User user = new User();

            if(!userRepository.existsUserByEmail(s.getMail())){
                user.setPassword(createCode(5));
                user.setEmail(s.getMail());
                user = userRepository.save(user);
            }else{
                user = userRepository.findUserByEmail(user.getEmail());
            }

            UserTournamentRole userTournamentRole = new UserTournamentRole();
            userTournamentRole.setTournament(tournament);
            userTournamentRole.setUser(user);
            userTournamentRole.setRole(UserRole.STAFF);
            userTournamentRoles.add(userTournamentRole);

        }

        userTournamentRoleRepository.saveAll(userTournamentRoles);

        return "redirect:/tournament/view?tournamentId=" + tournament.getId();

    }

    boolean areAllDistinct(AddStaffList staff) {
        return staff.getMails().stream().distinct().count() == staff.getMails().size();
    }

    public String createCode(int n){
        return RandomStringUtils.randomAlphabetic(n);
    }
}
