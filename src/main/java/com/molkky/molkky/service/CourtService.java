package com.molkky.molkky.service;

import com.molkky.molkky.domain.Court;
import com.molkky.molkky.model.CourtModel;
import com.molkky.molkky.repository.CourtRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class CourtService {
    @Autowired
    private CourtRepository courtRepository;

    public Court getCourtFromModel(CourtModel courtModel){
        return courtRepository.findById(courtModel.getId());
    }

    public List<Court> getAvailableCourts(){
        return courtRepository.findByAvailable(true);
    }
}
