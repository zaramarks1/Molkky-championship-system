package com.molkky.molkky.service;

import com.molkky.molkky.MolkkyApplication;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest(classes = MolkkyApplication.class)
public class TournamentServiceTest {

    @Autowired
    private TounamentService tounamentService;

    @Autowired
    private TournamentRepository tournamentRepository;

    @Autowired
    private UserRepository userRepository;

    @Mock
    private TournamentModel tournamentModel;

    @Test
    public void testCreateCode(){
        int length = 5;

        String code = this.tounamentService.createCode(length);

        StringBuilder test = new StringBuilder();
        for (int i =0; i<length;i++){
            test.append("a");
        }

        Assertions.assertEquals(test.length(), code.length());
    }

    @Test
    public void createTournamentServiceWithoutUser(){
        String name = "test" + Math.random() * 10000;
        Mockito.when(tournamentModel.getName()).thenReturn(name);
        Tournament tournament = this.tounamentService.create(this.tournamentModel);
        Tournament tested = this.tournamentRepository.findByName(name);

        Assertions.assertEquals(tournament.getName(), tested.getName());
    }

    @Test
    public void createTournamentServiceWithUser(){
        String name = "test" + Math.random() * 10000;
        String email = "coucou@gmail.com";
        Mockito.when(tournamentModel.getEmail()).thenReturn(email);
        Mockito.when(tournamentModel.getName()).thenReturn(name);
        Tournament tournament = this.tounamentService.create(this.tournamentModel);

        if (!this.userRepository.existsUserByEmail(email)){
            User user = new User();
            user.setEmail(email);
            user.setPassword(this.tounamentService.createCode(5));
            this.userRepository.save(user);
        }

        Assertions.assertEquals(tournament.getName(), this.tournamentRepository.findByName(name).getName());

    }

}
