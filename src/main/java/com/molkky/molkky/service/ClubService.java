package com.molkky.molkky.service;

import com.molkky.molkky.domain.Club;
import com.molkky.molkky.repository.ClubRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Objects;

@Service
public class ClubService {
    @Autowired
    private ClubRepository clubRepository;

    public List<Club> getClubsByName(String name){
        return (List<Club>) clubRepository.findAll().stream().filter(i -> Objects.equals(i.getName(), name));
    }
}
