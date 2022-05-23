package com.molkky.molkky.service;

import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.domain.UserTournamentRole;
import com.molkky.molkky.model.AddPlayerModel;
import com.molkky.molkky.model.AddPlayerlistModel;
import com.molkky.molkky.model.CreateTeamModel;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;


import org.apache.commons.lang.RandomStringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import type.UserRole;

import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.List;


@Service
public class TeamService {

    @Autowired
    TeamRepository teamRepository;

    @Autowired
    UserRepository userRepository;

    @Autowired
    UserTournamentRoleRepository userTournamentRoleRepository;

    @Autowired
    TournamentRepository tournamentRepository;


    public Team create(CreateTeamModel team){

        Integer idTournament = team.getTournament();
        Tournament tournament = tournamentRepository.findById(idTournament);

        Team teamCreate = new Team();

        teamCreate.setName(team.getName());
        teamCreate.setTournament(tournament);

        teamCreate.setCode(createCode(5));

        return teamRepository.save(teamCreate);



    }

    public Team addPlayers(AddPlayerlistModel addPlayerlistModel){

        List<AddPlayerModel> players = addPlayerlistModel.getPlayers();
        List<UserTournamentRole> userTournamentRoles = new ArrayList<>();

        Team team = teamRepository.findById(players.get(0).getTeamId());
        for(AddPlayerModel player : players){

            User user = player.addPlayer();
           /* TODO user.setTeam(team);
            String pwd = user.getCode();
            //emailSenderService.SendEmail(user.getEmail(),"Votre code d'identification au site Molkky","Voici votre code : "+ pwd);
            PasswordEncoder passwordEncoder = new BCryptPasswordEncoder();
            String hashedPassword = passwordEncoder.encode(pwd);
            user.setCode(hashedPassword);*/

            if(!userRepository.existsUserByEmail(user.getEmail())){
                user.setPassword(createCode(5));
                user = userRepository.save(user);
            }else{
                user = userRepository.findUserByEmail(user.getEmail());
            }

            UserTournamentRole userTournamentRole = new UserTournamentRole();
            userTournamentRole.setTournament(team.getTournament());
            userTournamentRole.setUser(user);
            userTournamentRole.setTeam(team);
            userTournamentRole.setRole(UserRole.PLAYER);
            userTournamentRoles.add(userTournamentRole);



        }


        userTournamentRoleRepository.saveAll(userTournamentRoles);


        return team;
    }

    boolean areAllDistinct(List<User> users) {
        return users.stream().map(User::getEmail).distinct().count() == users.size();
    }

    public String createCode(int n){
        return RandomStringUtils.random(n, 0, 0, true, true, null, new SecureRandom());
    }
}
